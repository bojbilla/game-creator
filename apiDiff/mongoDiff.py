#!/usr/bin/python3
import requests
import sys
import json
import re
import os
import signal
import time
import ast
import psutil
from subprocess import Popen, call
from pymongo import MongoClient
from bson.json_util import dumps

game_creator_api = {
    "fetch": "http://localhost:9900/fetchData?user_id={}&access_token={}",
    "wipe": "http://localhost:9900/dropDatabase"
}
game_creator_directory = "C:/Users/bapti/Documents/Work/Semester Project/reminisceme/game-creator"
api_file_path = game_creator_directory+"/src/main/scala/me/reminisce/fetching/config/FacebookServiceConfig.scala"
dir = "jsonDir/"
FNULL = open(os.devnull, 'w')

def main():
    if(len(sys.argv) != 5):
        print("USAGE:\n python mongoJsonDiff.py <fb api version> <fb api version> <user id> <access_token>" , sys.stderr)
        sys.exit(1)

    versions = [sys.argv[1], sys.argv[2]]
    user = sys.argv[3]
    access_token = sys.argv[4]

    print("[INFO]: Empty jsonDir")
    filelist = [ dir+f for f in os.listdir(dir) if f.endswith(".json")]
    for f in filelist:
        os.remove(f)
    print("[INFO]: Start mongodb")
    mongo = "mongod"
    if('win' in sys.platform):
        mongo = mongo+".exe"
        
    mongo = Popen([mongo],stdout=FNULL)
    pid_mongo = mongo.pid 

    [jsons1,jsons2] = [get_jsons_from_db(v,user,access_token) for v in versions]

    print("[INFO]: Kill mongo")
    mongo.kill()
    print("[INFO]: Start json diff")
    for c in jsons1.keys():
        if(c in jsons2.keys()):
            f = open(dir+c+"_diff.json","w")
            diffcmd = "jsondiffpatch"    
            if('win' in sys.platform):
                diffcmd = diffcmd+".cmd"
            try:
                call([diffcmd, jsons1[c], jsons2[c]], stdout=f) 
            except:
                sys.exc_info()[0]
                print("[INFO]: Please install jsondiffpatch: `npm install jsondiffpatch`", sys.stderr)
                time.sleep(5)
                raise
#      
    print("[INFO]: jsons diff available")
    sys.exit()
    


def get_jsons_from_db(version,user,access_token):
    """
    1) Modify the api version
    2) Start sbt run
    3) Wipe the db
    4) Fetch data for the given user and given access token
    5) Write the db's collections as json
    
    :version the version on which to launch the game_creator
    :user the user id
    :access_token the user's access token
    
    :return dict of collection names -> file name
    """
    print("[INFO]: Modify API version with "+version)
    change_api_version(version)
    print("[INFO]: Start sbt run")
    sbt = Popen(["sbt", "run"],cwd=game_creator_directory, shell=True)
    pid_sbt = sbt.pid
    
    try:
       sbt.wait(timeout=60)
    except:
        print('[INFO]: Sbt should be almost ready')
   
    print("[INFO]: Wipe db")
    again = True
    while(again):
        try:
            wipe()
            again=False
        except:
            print('[INFO]: Wait on sbt to launch')
            

    print("[INFO]: Fetch user data")
    fetch(user, access_token)
    try:
       sbt.wait(timeout=30)
    except:
        print('[INFO]: Fetching Done')
        
    client = MongoClient('localhost', 27017)
    db = client.game_creator
    collections = db.collection_names()
    collections.remove('lastFetched')
    print(collections)
    collection2file = {}
    print("[INFO]: Write jsons from db")
    for c_names in collections:
        cursor_as_list = list(db[c_names].find({},{'_id': 0 }))
        file = dir+c_names+version+".json"
        collection2file[c_names] = file
        with open(file, 'w') as fp:
            json.dump(cursor_as_list, fp, indent=4, default=json_serial)
    print("[INFO]: Wipe db")
    wipe()
    print("[INFO]: Kill sbt run process and its children")
    kill_sbt(pid_sbt)
    
    return collection2file
    
def kill_sbt(pid_sbt):
    """
    Kills the sbt process and its children (doesn't work with os.kill or Popen.kill)
    
    :pid_sbt sbt's pid
    """
    parent = psutil.Process(pid_sbt)
    children = parent.children(recursive=True)
    for process in children:
        process.send_signal(signal.SIGTERM)
        
        
def change_api_version(version):
    """
    Modify the version of the fb api in game_creator's FacebookServiceConfig.scala 
    
    :version the version to be overwritten
    """
    with open(api_file_path, "r+") as f:
        old = f.read()
        res = re.sub("v\d+\.\d+", version, old)
        f.close()
    with open(api_file_path,"w+") as f:
        f.write(res)
        f.close()



def fetch(user_id, user_token):
    """
    Builds a fetch request URL based on the user id and access token
    :param user_id: user id
    :param user access token
    """
    response = requests.get(game_creator_api["fetch"].format(user_id, user_token))
    print_response(response)
    
def wipe():
    """
    Request the game creator to wipe its database.
    """
    print("Deletion...")    
    delete_response = requests.delete(game_creator_api["wipe"])
    print_response(delete_response)

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

from datetime import datetime

def json_serial(obj):
    """JSON serializer for objects not serializable by default json code"""

    if isinstance(obj, datetime):
        serial = obj.isoformat()
        return serial
    raise TypeError ("Type not serializable")

if __name__ == '__main__':
    main()
