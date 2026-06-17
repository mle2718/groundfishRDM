/* small helper to setup up users files for the data directory.
Ideally all data is stored somewhere in "Data" inside the repo but not committed
Most people will store data there.
The processed data takes up alot of space, so whoever processes the data will need to store it elsewhere.
*/

assert inlist("$user", "LCH", "TP", "ML", "KB")

if inlist("$user","LCH") {
	global datadir "E:\Lou_projects\groundfishRDM\2027_mgt_cycle"
} 
else if inlist("$user","TP", "ML","KB"){
	global datadir "${here}\Data"
}



display "Hello $user.  Use the global datadir in place of \${here}\Data)."
display "The value of datadir is: $datadir"


