-- https://www.codewars.com/kata/560248d6ba06815d6f000098
-- Reversed Self Power

module Power where

import Data.List
import Data.Maybe

minLengthNum  :: Int -> Int -> (Bool,Int)
minLengthNum numdig ordmax = search 1
    where
        search :: Int -> (Bool, Int)
        search i = let
            dig = length $ show $ s $ fromIntegral (i-1)
            in
                if i > ordmax then (False, -1)
                else if numdig == dig then (True, i)
                else if numdig < dig then (False, -1)
                else search (i+1)

-- self reversed power sequence
s :: Integer -> Integer
s 0 = 0
s n = sum [ i ^ (n-i+1) | i <- [1..n] ]

ss = [1,1,1,1,2,2,3,3,4,5,5,6,7,7,8,9,10,10,11,12,13,14,15,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,44,45,46,47,48,49,51,52,53,54,55,57,58,59,60,61,63,64,65,66,68,69,70,72,73,74,75,77,78,79,81,82,83,85,86,87,88,90,91,93,94,95,97,98,99,101,102,103,105,106,108,109,110,112,113,114,116,117,119,120,122,123,124,126,127,129,130,132,133,134,136,137,139,140,142,143,145,146,148,149,150,152,153,155,156,158,159,161,162,164,165,167,168,170,171,173,174,176,177,179,180,182,184,185,187,188,190,191,193,194,196,197,199,201,202,204,205,207,208,210,211,213,215,216,218,219,221,222,224,226,227,229,230,232,234,235,237,238,240,242,243,245,246,248,250,251,253,255,256,258,259,261,263,264,266,268,269,271,273,274,276,277,279,281,282,284,286,287,289,291,292,294,296,297,299,301,302,304,306,307,309,311,312,314,316,317,319,321,322,324,326,328,329,331,333,334,336,338,339,341,343,345,346,348,350,351,353,355,357,358,360,362,363,365,367,369,370,372,374,376,377,379,381,383,384,386,388,389,391,393,395,396,398,400,402,403,405,407,409,411,412,414,416,418,419,421,423,425,426,428,430,432,433,435,437,439,441,442,444,446,448,450,451,453,455,457,458,460,462,464,466,467,469,471,473,475,476,478,480,482,484,485,487,489,491,493,495,496,498,500,502,504,505,507,509,511,513,515,516,518,520,522,524,526,527,529,531,533,535,537,538,540,542,544,546,548,549,551,553,555,557,559,561,562,564,566,568,570,572,573,575,577,579,581,583,585,587,588,590,592,594,596,598,600,601,603,605,607,609,611,613,615,616,618,620,622,624,626,628,630,632,633,635,637,639,641,643,645,647,649,650,652,654,656,658,660,662,664,666,668,669,671,673,675,677,679,681,683,685,687,689,690,692,694,696,698,700,702,704,706,708,710,712,713,715,717,719,721,723,725,727,729,731,733,735,737,739,740,742,744,746,748,750,752,754,756,758,760,762,764,766,768,770,771,773,775,777,779,781,783,785,787,789,791,793,795,797,799,801,803,805,807,809,811,813,814,816,818,820,822,824,826,828,830,832,834,836,838,840,842,844,846,848,850,852,854,856,858,860,862,864,866,868,870,872,874,876,878,880,882,884,886,888,890,892,894,896,898,900,902,904,906,908,910,911,913,915,917,919,921,923,925,927,929,931,934,936,938,940,942,944,946,948,950,952,954,956,958,960,962,964,966,968,970,972,974,976,978,980,982,984,986,988,990,992,994,996,998,1000,1002,1004,1006,1008,1010,1012,1014,1016,1018,1020,1022,1024,1026,1028,1030,1032,1034,1037,1039,1041,1043,1045,1047,1049,1051,1053,1055,1057,1059,1061,1063,1065,1067,1069,1071,1073,1075,1077,1079,1081,1083,1086,1088,1090,1092,1094,1096,1098,1100,1102,1104,1106,1108,1110,1112,1114,1116,1118,1120,1123,1125,1127,1129,1131,1133,1135,1137,1139,1141,1143,1145,1147,1149,1151,1153,1156,1158,1160,1162,1164,1166,1168,1170,1172,1174,1176,1178,1180,1183,1185,1187,1189,1191,1193,1195,1197,1199,1201,1203,1205,1207,1210,1212,1214,1216,1218,1220,1222,1224,1226,1228,1230,1232,1235,1237,1239,1241,1243,1245,1247,1249,1251,1253,1255,1258,1260,1262,1264,1266,1268,1270,1272,1274,1276,1279,1281,1283,1285,1287,1289,1291,1293,1295,1297,1300,1302,1304,1306,1308,1310,1312,1314,1316,1319,1321,1323,1325,1327,1329,1331,1333,1335,1338,1340,1342,1344,1346,1348,1350,1352,1354,1357,1359,1361,1363,1365,1367,1369,1371,1374,1376,1378,1380,1382,1384,1386,1388,1391,1393,1395,1397,1399,1401,1403,1405,1408,1410,1412,1414,1416,1418,1420,1422,1425,1427,1429,1431,1433,1435,1437,1440,1442,1444,1446,1448,1450,1452,1455,1457,1459,1461,1463,1465,1467,1470,1472,1474,1476,1478,1480,1482,1485,1487,1489,1491,1493,1495,1497,1500,1502,1504,1506,1508,1510,1512,1515,1517,1519,1521,1523,1525,1528,1530,1532,1534,1536,1538,1540,1543,1545,1547,1549,1551,1553,1556,1558,1560,1562,1564,1566,1569,1571,1573,1575,1577,1579,1582,1584,1586,1588,1590,1592,1595,1597,1599,1601,1603,1605,1608,1610,1612,1614,1616,1618,1621,1623,1625,1627,1629,1631,1634,1636,1638,1640,1642,1645,1647,1649,1651,1653,1655,1658,1660,1662,1664,1666,1668,1671,1673,1675,1677,1679,1682,1684,1686,1688,1690,1693,1695,1697,1699,1701,1703,1706,1708,1710,1712,1714,1717,1719,1721,1723,1725,1728,1730,1732,1734,1736,1739,1741,1743,1745,1747,1750,1752,1754,1756,1758,1760,1763,1765,1767,1769,1771,1774,1776,1778,1780,1783,1785,1787,1789,1791,1794,1796,1798,1800,1802,1805,1807,1809,1811,1813,1816,1818,1820,1822,1824,1827,1829,1831,1833,1835,1838,1840,1842,1844,1847,1849,1851,1853]

minLengthNum'  :: Int -> Int -> (Bool,Int)
minLengthNum' numdig ordmax = let
    idx = fromMaybe (-1) (elemIndex numdig ss)
    in if idx > ordmax || idx == -1 then (False, -1) else (True, idx)

{-
2つの引数(ord maxとnum dig)を取り、インデックスがord max以下で、
正確にnum dig桁数を持つシーケンスの最小項を見つける関数を実装します。
正しい桁数の数字がある場合、結果は次の形式の配列である必要があります。

[true, smallest found term]
[false, -1]

ちょっと何言ってるかわからないが、例を見ることでようやく理解できる。

例：

minLengthNum(5, 10) === [true, 10]   // 10th term has 5 digits
minLengthNum(7, 11) === [false, -1]  // no terms before the 13th one have 7 digits
minLengthNum(7, 14) === [true, 13]   // 13th term is the first one which has 7 digits
下の表で見ることができます。

n-th Term    Term Value
1              0
2              1
3              3
4              8
5              22
6              65
7              209
8              732
9              2780
10             11377   <- ここで５桁に達した
11             49863
12             232768
13             1151914 <- ここで７桁に達した
14             6018785

１つ目の引数は桁数、２つ目の引数はその値以下の回数で指定した桁に達したらtrue、
そうでなければfalseということらしい。

解き方とすれば、２つ目の引数で指定した回数を上限として、
再起呼び出しでSを計算すればいい。

それで実装してみたが、どうにもうまく通らないケースがある。
どうやら、桁が２つ増えるケースがあるらしく、
numdigに発生しえない桁数が指定された場合は(False,-1)を返す必要があるようだ。

それでもタイムアウトになってしまうので、
最初に1000まで計算した結果を用意した。

-}