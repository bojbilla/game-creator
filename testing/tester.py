#!/usr/bin/python3
import os
import argparse
import requests
from time import sleep
import json
import multiprocessing

facebook_api_path = "https://graph.facebook.com/v2.2"

# Path to get an access token for the application administration
# requires the app id and the app secret
app_access_token_path = facebook_api_path + "/oauth/access_token?client_id={}&client_secret={}" \
                                            "&grant_type=client_credentials"

# Path to get the test users list, requires an access token for the application
test_users_list_path = facebook_api_path + "/{}/accounts/test-users?access_token={}"

users_list = []

# Access paths to a locally deployed game creator, the wipe route only works if it was deployed in a "DEV" environment
game_creator_api = {
    "fetch": "http://localhost:9900/fetchData?user_id={}&access_token={}",
    "board": "http://localhost:9900/gameboard?user_id={}&access_token={}&strategy={}",
    "wipe": "http://localhost:9900/dropDatabase"
}


def setup_users_list():
    """
     Uses the app id and app secret to get a list of the test users with their access tokens.
     The environment variables "FACEBOOK_APP_ID" and "FACEBOOK_APP_SECRET" must be setup properly otherwise the script
     fails.
    """
    global app_access_token_path, test_users_list_path, users_list
    facebook_app_id = os.environ.get("FACEBOOK_APP_ID")
    
    if facebook_app_id is None:
        print("Facebook app id is not defined.")
        return 1

    facebook_app_secret = os.environ.get("FACEBOOK_APP_SECRET")
    if facebook_app_secret is None:
        print("Facebook app id is not defined.")
        return 1

    app_access_token_path = app_access_token_path.format(facebook_app_id, facebook_app_secret)

    app_access_token_resp = requests.get(app_access_token_path)
    if not check_response(app_access_token_resp, "Facebook did not send access_token."):
        return 2

    split_resp = app_access_token_resp.text.split('=')
    if len(split_resp) < 2:
        print("Response format unexpected:", app_access_token_resp.text)
        return 3

    app_access_token = split_resp[1]

    if app_access_token is None or app_access_token == "":
        print("Empty access token.")
        return 3

    test_users_list_path = test_users_list_path.format(facebook_app_id, app_access_token)

    test_users_list_resp = requests.get(test_users_list_path)

    if not check_response(test_users_list_resp, "Facebook did not send list of test users."):
        return 2

    users_list = test_users_list_resp.json()["data"]
    return 0


def check_response(response, error_mess):
    """
    Check the status code of the http response and returns True iff the status was 200.
    :param response: HTTP response
    :param error_mess: error message to print if the status code is not 200
    :return: response.status_code == 200
    """
    if response.status_code != 200:
        print(error_mess, "Error code:", response.status_code, ", message:", response.text)
        return False
    return True


def print_response(response, print_text=True, fields=None):
    """
    HTTP response pretty printing
    :param response: HTTP response to print
    :param print_text: enables the printing of the response body entirely
    :param fields: if print_text is False, fields of the JSON response to print
    """
    print("Status code:", response.status_code)

    json_response = json.loads(response.text)
    text = json.dumps(json_response, indent=4, separators=(',', ':'))

    if print_text:
        print("Text:")
        print(text)
    elif fields is not None:
        for f in fields:
            if f in json_response:
                print(f + ": " + json_response[f])
            else:
                print("Field \"" + f + "\" not found.")


def fetch_request(user):
    """
    Builds a fetch request URL based on the user id and access token
    :param user: dict containing the fields "id" and "access_token"
    :return: the built request URL
    """
    user_id = user["id"]
    user_token = user["access_token"]
    return game_creator_api["fetch"].format(user_id, user_token)


def fetch_for_users():
    """
    Performs a fetch request to the game creator for each user in the users_list list.
    Uses a thread pool to parallelize the requests.
    """
    print("Requesting fetch...")
    # Builds a list of fetch request URLS
    fetch_requests = [fetch_request(user) for user in users_list]
    # Creates a thread pool with one thread per request
    p = multiprocessing.Pool(len(fetch_requests))
    # p.map(f, data) applies f to each item of data, using the threads in thread pool p and returns a list of the
    # results of the computation
    responses = p.map(requests.get, fetch_requests)
    for response in responses:
        print_response(response)


def board_request(user):
    """
    Builds a game board request URL based on the user id and access token
    :param user: dict containing the fields "id" and "access_token"
    :return: the built request URL
    """
    user_id = user["id"]
    user_token = user["access_token"]
    return game_creator_api["board"].format(user_id, user_token, "chooser")


def boards(args):
    """
    Performs a game board request to the game creator for each user in the users_list list.
    Uses a thread pool to parallelize the requests.
    :param args: namespace containing additional arguments for pretty printing (verbose to enable/disable printing the
    whole game board body, board_query specifies fields to print otherwise)
    """
    # See thread pool usage explained in the fetch_for_users() function
    board_requests = [board_request(user) for user in users_list]
    p = multiprocessing.Pool(len(board_requests))
    responses = zip(p.map(requests.get, board_requests), users_list)
    fields = args.board_query if "board_query" in args else None
    for response in responses:
        print("Board for user", response[1]["id"])
        print_response(response[0], args.verbose, fields)
        print()


def wipe():
    """
    Request the game creator to wipe its database.
    """
    print("Deletion...")
    delete_response = requests.delete(game_creator_api["wipe"])
    print_response(delete_response)


def check_positive_float(value):
    """
    Checks if the string value is a valid positive floating point number and returns it. Used for arguments parsing.
    :param value: string to check
    :raises argparse.ArgumentTypeError: if value is not a valid floating point number or not a positive floating point
    number
    :return: value as a floating point number
    """
    try:
        float_value = float(value)
    except ValueError:
        raise argparse.ArgumentTypeError(value + " is not a floating point number.")
    if float_value < 0.0:
        raise argparse.ArgumentTypeError(value + " is not a positive number.")
    return float_value


def get_parser():
    """
    Creates an arguments parser used to parse this scripts arguments. The usage of each flag is specified here.
    :return: the generated arguments parser
    """
    parser = argparse.ArgumentParser(description="Tool to test the game creator. The actions are executed in the"
                                                 " following order: wipe, fetch, board.")
    parser.add_argument("-f", "--fetch", action="store_true", help="Fetches data for all the test users.")
    parser.add_argument("-b", "--board", action="store_true", help="Generates game boards for test users.")
    parser.add_argument("-t", "--wait-time", type=check_positive_float, help="Sets a waiting time (in seconds) between "
                                                                             "last fetch request and the board "
                                                                             "requests.")
    parser.add_argument("-v", "--verbose", action="store_true", help="Prints the generated game boards.")
    parser.add_argument("-w", "--wipe", action="store_true", help="Wipes the database.")
    parser.add_argument("-q", "--board-query", nargs="+", help="Fields of board to print", required=False)
    return parser


def main():
    """
    Main function in this script. Parses the argument, sets up the users list with the access tokens, then performs
    actions in the following order:
    1. Requests a database wipe of the wipe flag is enabled
    2. Requests data fetching for each user if the fetch flag is enabled. If the boards flag is also enabled, sleeps
       for the specified wait_time (if specified).
    3. Requests a game board for each user if the board flag is enabled. The specified elements in board_query are used
       to print particular fields instead of the whole game board.
    :return: 0 if nothing failed
    """
    parser = get_parser()
    args = parser.parse_args()

    if not args.fetch and not args.board and not args.wipe:
        parser.print_help()
        return 0

    setup_error = setup_users_list()

    if setup_error != 0:
        return setup_error

    if args.wipe:
        wipe()
    
    if args.fetch:
        fetch_for_users()
        if args.board:
            if "wait_time" in args and args.wait_time is not None:
                sleep(args.wait_time)

    if args.board:
        boards(args)
    return 0

if __name__ == "__main__":
    main()

