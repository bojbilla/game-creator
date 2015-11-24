#!/usr/bin/python3
import os
import argparse
import requests
from time import sleep
import json
import multiprocessing

facebook_api_path = "https://graph.facebook.com/v2.2"

app_access_token_path = facebook_api_path + "/oauth/access_token?client_id={}&client_secret={}" \
                                            "&grant_type=client_credentials"

test_users_list_path = facebook_api_path + "/{}/accounts/test-users?access_token={}"

users_list = []

game_creator_api = {
    "fetch": "http://localhost:9900/fetchData?user_id={}&access_token={}",
    "board": "http://localhost:9900/gameboard?user_id={}&access_token={}&strategy={}",
    "wipe": "http://localhost:9900/dropDatabase"
}


def setup_users_list():
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
    if response.status_code != 200:
        print(error_mess, "Error code:", response.status_code, ", message:", response.text)
        return False
    return True


def print_response(response, print_text=True, fields=None):
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
                print("Field", f, "not found.")


def fetch_request(user):
    user_id = user["id"]
    user_token = user["access_token"]
    return game_creator_api["fetch"].format(user_id, user_token)


def fetch_for_users():
    print("Requesting fetch...")
    fetch_requests = [fetch_request(user) for user in users_list]
    p = multiprocessing.Pool(len(fetch_requests))
    responses = p.map(requests.get, fetch_requests)
    [print_response(response) for response in responses]


def board_request(user):
    user_id = user["id"]
    user_token = user["access_token"]
    return game_creator_api["board"].format(user_id, user_token, "chooser")


def boards(args):
    board_requests = [board_request(user) for user in users_list]
    p = multiprocessing.Pool(len(board_requests))
    responses = zip(p.map(requests.get, board_requests), users_list)
    fields = args.board_query if "board_query" in args else None
    for response in responses:
        print("Board for user", response[1]["id"])
        print_response(response[0], args.verbose, fields)
        print()


def wipe():
    print("Deletion...")
    delete_response = requests.delete(game_creator_api["wipe"])
    print_response(delete_response)


def check_positive_float(value):
    try:
        float_value = float(value)
    except ValueError:
        raise argparse.ArgumentTypeError(value + " is not a floating point number.")
    if float_value < 0.0:
        raise argparse.ArgumentTypeError(value + " is not a positive number.")
    return float_value


def get_parser():
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

