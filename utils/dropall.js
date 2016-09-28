// run this with
// $ mongo dropall.js

var dbs = db.getMongo().getDBNames();
for(var i in dbs){
	db = db.getMongo().getDB( dbs[i] );
	db.dropDatabase();
}
