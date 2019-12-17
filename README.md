# geodude
## Usage
1. Main.hs

Accepts a path of .txt file which contains lines of GPS coordinates and the mode the program should run with, and then returns coordinates togethering with the country and state it belongs to.

```
./Main <filename> <mode>
```
Mode could only be "s" (Sequential) or "p" (Parallel).
For example,
```
./Main "../data/testPoints.txt" s 
```
returns 
```
((-69.95441436767578,12.518703864466934),[Country{ "Aruba"},State{ Just "Aruba"}])
((84.0234375,33.7243396617476),[State{ Just "Xizang"},Country{ "China"}])
((36.2109375,57.136239319177434),[State{ Just "Tver'"},Country{ "Russia"}])
((10.1953125,50.736455137010665),[State{ Just "Th\252ringen"},Country{ "Germany"}])
((22.8515625,-6.315298538330033),[State{ Just "Kasa\239-Occidental"},Country{ "Congo (Kinshasa)"}])
((112.8515625,37.43997405227057),[State{ Just "Shanxi"},Country{ "China"}])
((-103.35937499999999,39.36827914916014),[State{ Just "Colorado"},Country{ "United States"}])
((-119.17968749999999,55.97379820507658),[State{ Just "Alberta"},Country{ "Canada"}])
((92.46093749999999,63.54855223203644),[State{ Just "Krasnoyarsk"},Country{ "Russia"}])
((131.8359375,-23.24134610238612),[State{ Just "Northern Territory"},Country{ "Australia"}])
```

2. evaluatePerformance.hs

Accepts loadMode, buildTreeMode, queryMode, numPoint, numEntity five args and runs program with numPoint points and numEntity entities under specific modes. The main purpose of this function is to evaluate the effectiveness of parallism for different part of the program, so the final result is just the length of result list.

```
./evaluatePerformance <loadFile mode> <makeTree mode> <queryPoint mode> <numPoint> <numPolygon>
``` 
Mode could only be "s" (Sequential) or "p" (Parallel).
