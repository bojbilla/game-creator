diffVersion.py:
-------------------
 - USAGE:
  
    `python diffVersion.py <fb api version> <fb api version> <user id> <access_token>`
  
 - Description:
  
    Query the facebook api with the 3 different queries present in the game-creator for
    the given user and the given versions.
    Writes in "./jsonDir/" the json file queried and the diff between the 2 versions provided. 
  
 - Example:
  
    `python diffVersion.py v2.3 v2.7 201247182312 ASDHCKJajsdkjbAS981323gJC`

mongoJsonDiff.py
--------------------
 - USAGE:
    
    `python mongoJsonDiff.py <fb api version> <fb api version> <user id> <access_token>`
    
 - Description:
 
    Build and run the game_creator with the given 2 fb api versions, fetch the data for the given user,
    save all the collections in the mongodb as JSON files, and finally apply a diff between the collections
    to see the changes that are applied by the change of fb api version.
    
 - Example:
  
    `python mongoJsonDiff.py v2.3 v2.7 201247182312 ASDHCKJajsdkjbAS981323gJC`
