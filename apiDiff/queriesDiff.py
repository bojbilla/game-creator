#!/usr/bin/python3
import requests
import sys
import json
import re
import ast
import os
from subprocess import call

def main():
    if(len(sys.argv) != 5):
        print("USAGE:\n python diffVersion.py <fb api version> <fb api version> <user id> <access_token>" , sys.stderr)
        sys.exit(1)
        
    
    versions = [sys.argv[1], sys.argv[2]]
    user = sys.argv[3]
    access_token = sys.argv[4]
    
 
    queries = {"like":"/likes?fields=name,photos.type(profile).limit(1){id,name,source},created_time,likes",
                "posts":"/posts?fields=likes.limit(1000).summary(true),message,created_time,link,from,type,story,source,full_picture,attachments{id,description,media{image},type},place,comments.limit(1000).summary(true){created_time,from,message,attachment,like_count}",
                "tag":"/tagged?fields=id,message,created_time,link,from,likes.limit(1000).summary(true),source,place"}
    dir = "jsonDir/"
    
    os.mkdir(dir, 666)
        
    # Query the fb api    
    for qi in queries.keys():
        for v in versions:
            q = "https://graph.facebook.com/"+v+"/"+user+queries[qi]+"&access_token="+access_token
            res = requests.get(q)
            if(str(res) != '<Response [200]>'):
                print(res)
                sys.exit()
            
            # this string will always diff since it always contains the api version, so we remove it
            rep = re.sub('\'https\:\/\/graph\.facebook\.com\/.*?\'', '\'REPLACED\'', str(res.json()))  
            rep_dict = ast.literal_eval(rep)
            
            with open(dir+qi+v+".json", 'w') as fp:
                json.dump(rep_dict, fp, indent=4)            
        
        f = open(dir+qi+"_diff.json","w")
        
        diffcmd = "jsondiffpatch"    
        if('win' in sys.platform):
            diffcmd = diffcmd+".cmd"
        
        call([diffcmd,dir+qi+versions[0]+".json",dir+qi+versions[1]+".json"], stdout=f) 
        
    print("Resulting diffs available!")
        
            
     
if __name__ == '__main__':
    main()