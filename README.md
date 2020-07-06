# UNEtracker # Project to handle and process data files from UNEtracker GPS collars #

#1. To use the script you need .txt files containing RAW GGA and RMC NMEA strings together, and that is all! 

#2. Both sets of strings should be in the same file and chronologically ordered. GGA and RMC strings should be matching pairs

#3. Individual strings should be comma separated, stored in new lines and begin with the NMEA code identifier 

#4. Download and "source" the script file after installing the required libraries to start  

#5. Output is written to an "Output" folder in the directory with you raw data files

#6. UNEtracker chipsets have an issue post mid 2019 where they incorrectly parse the date and time. As long as your data is collected post mid 2019, this shouldn't affect you, please doublecheck the calculated date-time fields

#7. Be aware script calculates AEST ("Brisbane/Australia") from UTC time, change your timezone if you are working elsewhere. Selectable UTM Zones cover Australia, but can be altered.

Input Data Example:

$GPGGA,065214.00,3028.9384,S,15138.0482,E,1,08,0.9,990.9,M,32.6,M,,*75
$GPGGA,092202.00,3028.9354,S,15138.0492,E,1,10,0.9,994.1,M,32.6,M,,*73
$GPGGA,115203.00,3028.9378,S,15138.0482,E,1,09,1.4,990.1,M,32.6,M,,*73
$GPGGA,142212.00,3028.9372,S,15138.0435,E,1,06,1.3,992.5,M,32.6,M,,*79
$GPGGA,165232.00,3028.9454,S,15138.0482,E,1,05,2.2,990.9,M,32.6,M,,*7E
$GPGGA,192213.00,3028.9388,S,15138.0455,E,1,07,1.3,983.7,M,32.6,M,,*75
$GPGGA,215213.00,3028.9366,S,15138.0444,E,1,08,1.0,995.3,M,32.6,M,,*76
$GPGGA,002214.00,3028.9405,S,15138.0436,E,1,06,1.0,984.1,M,32.6,M,,*7E
$GPRMC,065214.00,A,3028.9384,S,15138.0482,E,0.00,0.0,110119,12.2,E,A*2B
$GPRMC,092202.00,A,3028.9354,S,15138.0492,E,0.00,0.0,110119,12.2,E,A*28
$GPRMC,115203.00,A,3028.9378,S,15138.0482,E,0.00,0.0,110119,12.2,E,A*28
$GPRMC,142212.00,A,3028.9372,S,15138.0435,E,0.00,0.0,110119,12.2,E,A*2C
$GPRMC,165232.00,A,3028.9454,S,15138.0482,E,0.00,0.0,110119,12.2,E,A*24
$GPRMC,192213.00,A,3028.9388,S,15138.0455,E,0.00,0.0,110119,12.2,E,A*23
$GPRMC,215213.00,A,3028.9366,S,15138.0444,E,0.00,0.0,110119,12.2,E,A*2F
$GPRMC,002214.00,A,3028.9405,S,15138.0436,E,0.00,0.0,120119,12.2,E,A*28
