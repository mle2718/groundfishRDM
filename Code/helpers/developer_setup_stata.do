/* small helper to setup up users files for the data directory.
Ideally all data is stored somewhere in "Data" inside the repo but not committed
Most people will store data there.
The processed data takes up alot of space, so whoever processes the data will need to store it elsewhere.
*/

assert inlist("$developer", "LCH", "TP", "ML", "KB")

if inlist("$developer","LCH") {
	global gfdatadir "E:\Lou_projects\groundfishRDM\2027_mgt_cycle"
} 
else if inlist("$developer","TP", "ML","KB"){
	global gfdatadir "${here}\Data\2027_mgt_cycle"
}
/* make this directory if it doesn't exist.*/
capture mkdir $gfdatadir 


display "Hello $developer.  Use the global gfdatadir in place of \${here}\Data\YYYY mgmt cycle)."
display "The value of datadir is: $gfdatadir"


