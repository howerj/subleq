#include <stdio.h> /* eForth for 16-bit SUBLEQ */
int main(void){short p=0,m[65536] = { 
0,0,50,11,4088,-32768,128,-2,-1,1,2,16,0,0,0,0,0,0,0,
15770,32256,3840,-443,0,0,0,0,0,0,0,0,64,15750,15714,14654,
15448,7566,0,15750,0,0,47,47,8168,0,0,32384,32384,32512,32512,
49,49,53,48,0,56,0,49,59,0,0,62,47,47,65,46,0,68,0,47,
71,0,0,74,44,44,77,36,0,80,0,44,83,0,0,86,13,13,89,44,
0,92,0,13,95,0,0,98,8,44,101,116,116,104,13,0,107,0,116,
110,0,0,113,15,15,116,0,0,119,0,15,122,0,0,125,13,13,128,
15,0,131,0,13,134,0,0,137,4,15,140,15,0,146,0,0,149,0,
0,164,163,163,152,13,0,155,0,163,158,0,0,161,0,0,164,8,
47,167,203,203,170,47,0,173,0,203,176,0,0,179,204,204,182,
47,0,185,0,204,188,0,0,191,210,210,194,47,0,197,0,210,200,
0,0,203,0,0,206,44,0,209,0,0,212,0,0,215,44,44,218,13,0,
221,0,44,224,0,0,227,0,0,86,0,0,-1,0,0,86,9,45,239,0,0,
86,8,45,245,0,0,86,12,12,251,45,12,254,45,45,257,12,0,260,
0,45,263,0,0,266,9,45,269,0,0,86,287,287,275,45,0,278,0,
287,281,0,0,284,45,45,287,0,0,290,0,45,293,0,0,296,0,0,
86,314,314,302,49,0,305,0,314,308,0,0,311,13,13,314,0,0,
317,0,13,320,0,0,323,359,359,326,45,0,329,0,359,332,0,0,
335,360,360,338,45,0,341,0,360,344,0,0,347,366,366,350,45,
0,353,0,366,356,0,0,359,0,0,362,13,0,365,0,0,368,0,0,371,
8,49,374,389,389,377,49,0,380,0,389,383,0,0,386,45,45,389,
0,0,392,0,45,395,0,0,398,8,49,401,0,0,86,45,-1,407,422,
422,410,49,0,413,0,422,416,0,0,419,45,45,422,0,0,425,0,45,
428,0,0,431,8,49,434,0,0,86,9,49,440,476,476,443,49,0,446,
0,476,449,0,0,452,477,477,455,49,0,458,0,477,461,0,0,464,
483,483,467,49,0,470,0,483,473,0,0,476,0,0,479,45,0,482,
0,0,485,0,0,488,-1,45,491,0,0,86,9,49,497,533,533,500,49,
0,503,0,533,506,0,0,509,534,534,512,49,0,515,0,534,518,0,
0,521,540,540,524,49,0,527,0,540,530,0,0,533,0,0,536,45,
0,539,0,0,542,0,0,545,560,560,548,44,0,551,0,560,554,0,0,
557,45,45,560,0,0,563,0,45,566,0,0,569,8,44,572,0,0,86,9,
49,578,614,614,581,49,0,584,0,614,587,0,0,590,615,615,593,
49,0,596,0,615,599,0,0,602,621,621,605,49,0,608,0,621,611,
0,0,614,0,0,617,45,0,620,0,0,623,0,0,626,641,641,629,44,
0,632,0,641,635,0,0,638,45,45,641,0,0,644,0,45,647,0,0,
650,8,44,653,20,0,656,0,45,659,0,0,662,20,0,665,0,45,668,
0,0,671,0,0,86,13,13,677,45,0,680,0,13,683,0,0,686,701,
701,689,49,0,692,0,701,695,0,0,698,45,45,701,0,0,704,0,45,
707,0,0,710,746,746,713,49,0,716,0,746,719,0,0,722,747,747,
725,49,0,728,0,747,731,0,0,734,753,753,737,49,0,740,0,753,
743,0,0,746,0,0,749,13,0,752,0,0,755,0,0,758,0,0,86,9,
49,764,800,800,767,49,0,770,0,800,773,0,0,776,801,801,779,
49,0,782,0,801,785,0,0,788,807,807,791,49,0,794,0,807,797,
0,0,800,0,0,803,45,0,806,0,0,809,0,0,812,0,0,86,830,830,
818,49,0,821,0,830,824,0,0,827,13,13,830,0,0,833,0,13,836,
0,0,839,9,49,842,878,878,845,49,0,848,0,878,851,0,0,854,
879,879,857,49,0,860,0,879,863,0,0,866,885,885,869,49,0,
872,0,885,875,0,0,878,0,0,881,45,0,884,0,0,887,0,0,890,
45,45,893,13,0,896,0,45,899,0,0,902,0,0,86,920,920,908,
49,0,911,0,920,914,0,0,917,45,45,920,0,0,923,0,45,926,0,
0,929,8,49,932,0,0,86,8,47,938,974,974,941,47,0,944,0,974,
947,0,0,950,975,975,953,47,0,956,0,975,959,0,0,962,981,981,
965,47,0,968,0,981,971,0,0,974,0,0,977,45,0,980,0,0,983,
0,0,986,1001,1001,989,49,0,992,0,1001,995,0,0,998,45,45,
1001,0,0,1004,0,45,1007,0,0,1010,8,49,1013,0,0,86,9,49,
1019,1055,1055,1022,49,0,1025,0,1055,1028,0,0,1031,1056,1056,
1034,49,0,1037,0,1056,1040,0,0,1043,1062,1062,1046,49,0,1049,
0,1062,1052,0,0,1055,0,0,1058,45,0,1061,0,0,1064,0,0,1067,
1082,1082,1070,47,0,1073,0,1082,1076,0,0,1079,45,45,1082,0,0,
1085,0,45,1088,0,0,1091,9,47,1094,0,0,86,1112,1112,1100,47,
0,1103,0,1112,1106,0,0,1109,44,44,1112,0,0,1115,0,44,1118,
0,0,1121,9,47,1124,0,0,86,1142,1142,1130,49,0,1133,0,1142,
1136,0,0,1139,13,13,1142,0,0,1145,0,13,1148,0,0,1151,45,
13,1154,45,45,1157,13,0,1160,0,45,1163,0,0,1166,8,49,1169,
0,0,86,1187,1187,1175,49,0,1178,0,1187,1181,0,0,1184,13,13,
1187,0,0,1190,0,13,1193,0,0,1196,13,0,1199,0,45,1202,0,0,
1205,8,49,1208,0,0,86,9,49,1214,1250,1250,1217,49,0,1220,0,
1250,1223,0,0,1226,1251,1251,1229,49,0,1232,0,1251,1235,0,0,
1238,1257,1257,1241,49,0,1244,0,1257,1247,0,0,1250,0,0,1253,
45,0,1256,0,0,1259,0,0,1262,1277,1277,1265,47,0,1268,0,1277,
1271,0,0,1274,45,45,1277,0,0,1280,0,45,1283,0,0,1286,0,0,
86,9,47,1292,0,0,86,9,49,1298,1334,1334,1301,49,0,1304,0,
1334,1307,0,0,1310,1335,1335,1313,49,0,1316,0,1335,1319,0,0,
1322,1341,1341,1325,49,0,1328,0,1341,1331,0,0,1334,0,0,1337,
45,0,1340,0,0,1343,0,0,1346,45,45,1349,47,0,1352,0,45,1355,
0,0,1358,0,0,86,47,47,1364,45,0,1367,0,47,1370,0,0,1373,
1388,1388,1376,49,0,1379,0,1388,1382,0,0,1385,45,45,1388,0,
0,1391,0,45,1394,0,0,1397,8,49,1400,0,0,86,9,49,1406,1442,
1442,1409,49,0,1412,0,1442,1415,0,0,1418,1443,1443,1421,49,
0,1424,0,1443,1427,0,0,1430,1449,1449,1433,49,0,1436,0,1449,
1439,0,0,1442,0,0,1445,45,0,1448,0,0,1451,0,0,1454,45,45,
1457,49,0,1460,0,45,1463,0,0,1466,8,45,1469,0,0,86,49,49,
1475,45,0,1478,0,49,1481,0,0,1484,0,0,86,1502,1502,1490,47,
0,1493,0,1502,1496,0,0,1499,13,13,1502,0,0,1505,0,13,1508,
0,0,1511,13,0,1517,0,0,1523,0,0,1520,0,13,1613,9,13,1526,
1562,1562,1529,47,0,1532,0,1562,1535,0,0,1538,1563,1563,1541,
47,0,1544,0,1563,1547,0,0,1550,1569,1569,1553,47,0,1556,0,
1569,1559,0,0,1562,0,0,1565,13,0,1568,0,0,1571,0,0,1574,
1589,1589,1577,44,0,1580,0,1589,1583,0,0,1586,15,15,1589,0,
0,1592,0,15,1595,0,0,1598,44,44,1601,15,0,1604,0,44,1607,0,
0,1610,0,0,86,8,44,1616,9,47,1619,0,0,86,45,0,1625,0,45,
1628,0,0,1631,45,0,1634,0,45,1637,0,0,1640,45,0,1643,0,45,
1646,0,0,1649,45,0,1652,0,45,1655,0,0,1658,45,0,1661,0,45,
1664,0,0,1667,45,0,1670,0,45,1673,0,0,1676,45,0,1679,0,45,
1682,0,0,1685,45,0,1688,0,45,1691,0,0,1694,45,0,1697,0,45,
1700,0,0,1703,45,0,1706,0,45,1709,0,0,1712,45,0,1715,0,45,
1718,0,0,1721,45,0,1724,0,45,1727,0,0,1730,45,0,1733,0,45,
1736,0,0,1739,45,0,1742,0,45,1745,0,0,1748,13,13,1751,45,0,
1754,0,13,1757,0,0,1760,45,45,1763,0,0,1766,0,45,1769,0,0,
1772,13,0,1778,0,0,1784,0,0,1781,0,13,1796,45,45,1787,8,0,
1790,0,45,1793,0,0,1796,0,0,86,1814,1814,1802,44,0,1805,0,
1814,1808,0,0,1811,44,44,1814,0,0,1817,0,44,1820,0,0,1823,
0,0,86,13,13,1829,45,0,1832,0,13,1835,0,0,1838,15,15,1841,
0,0,1844,0,15,1847,0,0,1850,13,0,1856,0,0,1862,0,0,1859,
0,13,1874,15,15,1865,8,0,1868,0,15,1871,0,0,1874,9,13,1877,
0,13,1892,15,15,1883,8,0,1886,0,15,1889,0,0,1892,1907,1907,
1895,49,0,1898,0,1907,1901,0,0,1904,45,45,1907,0,0,1910,0,
45,1913,0,0,1916,8,49,1919,15,0,1925,0,0,1931,0,0,1928,0,
15,1937,8,44,1934,0,0,86,1952,1952,1940,44,0,1943,0,1952,
1946,0,0,1949,13,13,1952,0,0,1955,0,13,1958,0,0,1961,44,
44,1964,13,0,1967,0,44,1970,0,0,1973,0,0,86,13,13,1979,45,
0,1982,0,13,1985,0,0,1988,45,45,1991,0,0,1994,0,45,1997,0,
0,2000,0,13,2015,45,45,2006,8,0,2009,0,45,2012,0,0,2015,0,
0,86,13,13,2021,45,0,2024,0,13,2027,0,0,2030,45,45,2033,8,
0,2036,0,45,2039,0,0,2042,13,0,2048,0,0,2054,0,0,2051,0,
13,2066,45,45,2057,0,0,2060,0,45,2063,0,0,2066,9,13,2069,0,
13,2084,45,45,2075,0,0,2078,0,45,2081,0,0,2084,0,0,86,13,
13,2090,45,0,2093,0,13,2096,0,0,2099,45,45,2102,0,0,2105,
0,45,2108,0,0,2111,13,0,2117,0,0,2120,0,0,2132,45,45,2123,
8,0,2126,0,45,2129,0,0,2132,8,13,2135,13,0,2141,0,0,2144,
0,0,2156,45,45,2147,8,0,2150,0,45,2153,0,0,2156,0,0,86,
2174,2174,2162,49,0,2165,0,2174,2168,0,0,2171,13,13,2174,0,0,
2177,0,13,2180,0,0,2183,8,49,2186,45,13,2189,45,45,2192,0,0,
2195,0,45,2198,0,0,2201,13,0,2207,0,0,2210,0,0,2222,45,45,
2213,8,0,2216,0,45,2219,0,0,2222,0,0,86,2240,2240,2228,49,0,
2231,0,2240,2234,0,0,2237,13,13,2240,0,0,2243,0,13,2246,0,
0,2249,8,49,2252,45,13,2255,45,45,2258,0,0,2261,0,45,2264,
0,0,2267,0,13,2282,45,45,2273,8,0,2276,0,45,2279,0,0,2282,
0,0,86,45,0,2288,0,45,2291,0,0,2294,0,0,86,13,13,2300,11,
0,2303,0,13,2306,0,0,2309,14,14,2312,9,13,2315,13,0,2321,0,
0,2327,0,0,2324,0,13,2432,14,0,2330,0,14,2333,0,0,2336,18,
18,2339,45,0,2342,0,18,2345,0,0,2348,16,16,2351,0,0,2354,
0,16,2357,0,0,2360,18,0,2366,0,0,2369,0,0,2381,16,16,2372,
8,0,2375,0,16,2378,0,0,2381,8,18,2384,18,0,2390,0,0,2393,
0,0,2405,16,16,2396,8,0,2399,0,16,2402,0,0,2405,16,0,2411,
0,0,2417,0,0,2414,0,16,2420,8,14,2420,45,0,2423,0,45,2426,
0,0,2429,0,0,2312,45,45,2435,14,0,2438,0,45,2441,0,0,2444,
0,0,86,13,13,2450,11,0,2453,0,13,2456,0,0,2459,45,13,2462,
2477,2477,2465,49,0,2468,0,2477,2471,0,0,2474,45,45,2477,0,
0,2480,0,45,2483,0,0,2486,8,49,2489,14,14,2492,13,0,2498,0,
0,2504,0,0,2501,0,13,2612,14,0,2507,0,14,2510,0,0,2513,18,
18,2516,45,0,2519,0,18,2522,0,0,2525,16,16,2528,0,0,2531,0,
16,2534,0,0,2537,18,0,2543,0,0,2546,0,0,2558,16,16,2549,8,
0,2552,0,16,2555,0,0,2558,8,18,2561,18,0,2567,0,0,2570,0,
0,2582,16,16,2573,8,0,2576,0,16,2579,0,0,2582,16,0,2588,0,
0,2594,0,0,2591,0,16,2597,8,14,2597,45,0,2600,0,45,2603,0,
0,2606,9,13,2609,0,0,2492,45,45,2615,14,0,2618,0,45,2621,
0,0,2624,0,0,86,13,13,2630,11,0,2633,0,13,2636,0,0,2639,
14,14,2642,2657,2657,2645,49,0,2648,0,2657,2651,0,0,2654,15,
15,2657,0,0,2660,0,15,2663,0,0,2666,8,49,2669,13,0,2675,0,
0,2681,0,0,2678,0,13,2876,14,0,2684,0,14,2687,0,0,2690,18,
18,2693,45,0,2696,0,18,2699,0,0,2702,16,16,2705,0,0,2708,0,
16,2711,0,0,2714,18,0,2720,0,0,2723,0,0,2735,16,16,2726,8,
0,2729,0,16,2732,0,0,2735,8,18,2738,18,0,2744,0,0,2747,0,0,
2759,16,16,2750,8,0,2753,0,16,2756,0,0,2759,18,18,2762,15,0,
2765,0,18,2768,0,0,2771,17,17,2774,0,0,2777,0,17,2780,0,0,
2783,18,0,2789,0,0,2792,0,0,2804,17,17,2795,8,0,2798,0,17,
2801,0,0,2804,8,18,2807,18,0,2813,0,0,2816,0,0,2828,17,17,
2819,8,0,2822,0,17,2825,0,0,2828,16,0,2831,0,17,2834,0,0,
2837,17,0,2843,0,0,2849,0,0,2846,0,17,2852,8,14,2852,15,0,
2855,0,15,2858,0,0,2861,45,0,2864,0,45,2867,0,0,2870,9,13,
2873,0,0,2669,45,45,2879,14,0,2882,0,45,2885,0,0,2888,0,0,
86,13,13,2894,11,0,2897,0,13,2900,0,0,2903,14,14,2906,2921,
2921,2909,49,0,2912,0,2921,2915,0,0,2918,15,15,2921,0,0,2924,
0,15,2927,0,0,2930,8,49,2933,13,0,2939,0,0,2945,0,0,2942,
0,13,3173,14,0,2948,0,14,2951,0,0,2954,18,18,2957,45,0,
2960,0,18,2963,0,0,2966,16,16,2969,0,0,2972,0,16,2975,0,
0,2978,18,0,2984,0,0,2987,0,0,2999,16,16,2990,8,0,2993,0,
16,2996,0,0,2999,8,18,3002,18,0,3008,0,0,3011,0,0,3023,16,
16,3014,8,0,3017,0,16,3020,0,0,3023,18,18,3026,15,0,3029,
0,18,3032,0,0,3035,17,17,3038,0,0,3041,0,17,3044,0,0,3047,
18,0,3053,0,0,3056,0,0,3068,17,17,3059,8,0,3062,0,17,3065,
0,0,3068,8,18,3071,18,0,3077,0,0,3080,0,0,3092,17,17,3083,
8,0,3086,0,17,3089,0,0,3092,16,0,3095,0,17,3098,0,0,3101,
8,17,3104,16,16,3107,9,0,3110,0,16,3113,0,0,3116,17,0,3122,
0,0,3128,0,0,3125,0,17,3140,16,16,3131,0,0,3134,0,16,3137,
0,0,3140,16,0,3143,0,14,3146,0,0,3149,15,0,3152,0,15,3155,
0,0,3158,45,0,3161,0,45,3164,0,0,3167,9,13,3170,0,0,2933,
45,45,3176,14,0,3179,0,45,3182,0,0,3185,0,0,86,13,13,3191,
11,0,3194,0,13,3197,0,0,3200,14,14,3203,3218,3218,3206,49,0,
3209,0,3218,3212,0,0,3215,15,15,3218,0,0,3221,0,15,3224,0,
0,3227,8,49,3230,13,0,3236,0,0,3242,0,0,3239,0,13,3476,14,
0,3245,0,14,3248,0,0,3251,18,18,3254,45,0,3257,0,18,3260,
0,0,3263,16,16,3266,0,0,3269,0,16,3272,0,0,3275,18,0,3281,
0,0,3284,0,0,3296,16,16,3287,8,0,3290,0,16,3293,0,0,3296,
8,18,3299,18,0,3305,0,0,3308,0,0,3320,16,16,3311,8,0,3314,
0,16,3317,0,0,3320,18,18,3323,15,0,3326,0,18,3329,0,0,3332,
17,17,3335,0,0,3338,0,17,3341,0,0,3344,18,0,3350,0,0,3353,
0,0,3365,17,17,3356,8,0,3359,0,17,3362,0,0,3365,8,18,3368,
18,0,3374,0,0,3377,0,0,3389,17,17,3380,8,0,3383,0,17,3386,
0,0,3389,16,0,3392,0,17,3395,0,0,3398,10,0,3401,0,17,3404,
0,0,3407,16,16,3410,9,0,3413,0,16,3416,0,0,3419,17,0,3425,
0,0,3431,0,0,3428,0,17,3443,16,16,3434,0,0,3437,0,16,3440,
0,0,3443,16,0,3446,0,14,3449,0,0,3452,15,0,3455,0,15,3458,
0,0,3461,45,0,3464,0,45,3467,0,0,3470,9,13,3473,0,0,3230,
45,45,3479,14,0,3482,0,45,3485,0,0,3488,0,0,86,3506,3506,
3494,49,0,3497,0,3506,3500,0,0,3503,13,13,3506,0,0,3509,0,
13,3512,0,0,3515,15,15,3518,14,14,3521,9,0,3524,0,14,3527,
0,0,3530,13,0,3536,0,0,3539,0,0,3551,14,14,3542,0,0,3545,
0,14,3548,0,0,3551,14,0,3557,0,0,3563,0,0,3560,0,14,3572,
8,15,3566,45,13,3569,0,0,3518,45,0,3575,0,13,3578,0,0,3581,
9,15,3584,45,45,3587,15,0,3590,0,45,3593,0,0,3596,3632,3632,
3599,49,0,3602,0,3632,3605,0,0,3608,3633,3633,3611,49,0,3614,
0,3633,3617,0,0,3620,3639,3639,3623,49,0,3626,0,3639,3629,0,
0,3632,0,0,3635,13,0,3638,0,0,3641,0,0,3644,0,0,86,40,0,
3653,0,0,3659,0,0,3656,0,40,3662,0,0,86,3677,3677,3665,20,
0,3668,0,3677,3671,0,0,3674,13,13,3677,0,0,3680,0,13,3683,
0,0,3686,13,0,3692,0,0,3698,0,0,3695,0,13,4085,8,39,3701,
15,15,3704,20,0,3707,0,15,3710,0,0,3713,8,15,3716,3752,3752,
3719,15,0,3722,0,3752,3725,0,0,3728,3753,3753,3731,15,0,3734,
0,3753,3737,0,0,3740,3759,3759,3743,15,0,3746,0,3759,3749,
0,0,3752,0,0,3755,44,0,3758,0,0,3761,0,0,3764,8,15,3767,
3803,3803,3770,15,0,3773,0,3803,3776,0,0,3779,3804,3804,3782,
15,0,3785,0,3804,3788,0,0,3791,3810,3810,3794,15,0,3797,0,
3810,3800,0,0,3803,0,0,3806,45,0,3809,0,0,3812,0,0,3815,8,
15,3818,3854,3854,3821,15,0,3824,0,3854,3827,0,0,3830,3855,
3855,3833,15,0,3836,0,3855,3839,0,0,3842,3861,3861,3845,15,
0,3848,0,3861,3851,0,0,3854,0,0,3857,47,0,3860,0,0,3863,0,
0,3866,8,15,3869,3905,3905,3872,15,0,3875,0,3905,3878,0,0,
3881,3906,3906,3884,15,0,3887,0,3906,3890,0,0,3893,3912,3912,
3896,15,0,3899,0,3912,3902,0,0,3905,0,0,3908,49,0,3911,0,
0,3914,0,0,3917,8,15,3920,46,46,3923,13,0,3926,0,46,3929,
0,0,3932,6,0,3935,0,46,3938,0,0,3941,48,48,3944,46,0,3947,
0,48,3950,0,0,3953,6,0,3956,0,48,3959,0,0,3962,20,20,3965,
13,0,3968,0,20,3971,0,0,3974,8,13,3977,3992,3992,3980,13,0,
3983,0,3992,3986,0,0,3989,44,44,3992,0,0,3995,0,44,3998,0,0,
4001,8,13,4004,4019,4019,4007,13,0,4010,0,4019,4013,0,0,4016,
45,45,4019,0,0,4022,0,45,4025,0,0,4028,8,13,4031,4046,4046,
4034,13,0,4037,0,4046,4040,0,0,4043,47,47,4046,0,0,4049,0,
47,4052,0,0,4055,8,13,4058,4073,4073,4061,13,0,4064,0,4073,
4067,0,0,4070,49,49,4073,0,0,4076,0,49,4079,0,0,4082,8,13,
4085,0,0,86,0,8962,48,494,0,1097,8176,8962,49,494,1,1097,
8188,8963,12589,494,-1,1097,0,12546,43,242,1097,8212,12546,45,
236,1097,8222,11009,1172,1097,8232,11521,1127,1097,8240,26886,
30318,29285,116,248,1097,8248,25091,25977,230,1097,8262,25603,
28789,761,1097,8272,25604,28530,112,905,1097,8282,28420,25974,
114,815,1097,8294,29444,24951,112,674,1097,8306,29190,26739,
26217,116,2447,1097,8200,23299,23872,272,1097,8332,23299,23841,
299,1097,8342,27651,25203,1622,1097,8318,29443,16496,1403,1097,
8362,29443,8560,1472,1097,8372,12290,62,1976,1097,8382,12290,
61,2018,1097,8392,12290,60,2087,1097,8402,15361,2159,1097,8412,
15873,2225,1097,8420,12802,47,2297,1097,8428,28418,114,2627,1097,
8438,30723,29295,2891,1097,8448,24835,25710,3188,1097,8352,28677,
30049,25971,3647,1097,8458,28163,28783,1097,8480,16385,2297,272,
1097,8488,8449,2297,299,1097,8498,15364,27503,62,575,24,1097,
8468,15366,28005,29801,62,575,20,1097,8522,15365,25963,15993,
575,18,1097,8538,15366,25445,28520,62,575,26,1097,8552,15369,
26988,25972,24946,15980,575,22,1097,8568,15366,28515,25708,62,
494,72,1097,8508,25351,29301,25970,29806,494,62,1097,8602,29192,
28527,11636,28534,99,494,68,1097,8618,29700,26984,115,575,0,
1097,8636,28675,25697,4322,494,960,1172,1097,8650,8965,28534,
29539,494,8,1097,8666,25351,28271,25972,29816,494,46,1097,8680,
26628,29285,101,494,38,4246,1097,8696,25092,29537,101,575,10,
1097,8712,25603,27760,575,12,1097,8726,26627,25708,575,14,1097,
8738,29445,24948,25972,575,28,1097,8586,25355,27745,25193,24946,
26996,28271,494,42,1097,8750,25091,27500,494,82,1097,8784,29443,
29283,494,84,1097,8796,15875,28265,575,16,1097,8808,25090,108,
494,32,1097,8820,25350,25465,25964,115,494,78,1097,8832,26627,
30821,494,16,4360,4251,1097,8848,25607,25445,28009,27745,494,
10,4360,4251,1097,8864,23809,4103,4379,4251,1097,8884,23361,
4091,4379,4251,1097,8764,27908,28257,121,4091,4407,4251,1097,
8896,28163,28777,674,905,1097,8924,29700,25461,107,674,815,1097,
8936,16132,30052,112,761,1826,4483,761,1097,8950,29187,29807,935,
674,1016,674,1097,8968,11524,28530,116,4487,4487,1097,8984,12805,
29284,28783,905,905,1097,8998,12804,30052,112,815,815,1097,9012,
12291,15676,1976,2018,1097,9026,12291,15932,2018,2018,1097,9038,
15617,1127,2018,1097,9050,15362,62,4527,2018,1097,9060,15874,
61,2159,2018,1097,9072,15362,61,2225,2018,1097,9084,12291,15678,
2087,2018,1097,9096,29954,60,4510,2087,2018,674,2087,2018,4533,
935,2159,1016,4533,1097,9108,29954,62,674,4557,1097,9138,29955,
15678,4557,2018,1097,9150,29955,15676,4572,2018,1097,9162,28166,
26469,29793,101,236,248,1097,9174,29443,25662,761,2087,1097,
9190,24835,29538,4598,1826,4608,4592,1097,9202,12802,42,2285,
1097,9218,25348,27749,108,494,2,1097,9228,25349,27749,11116,
4618,1172,1097,9242,25349,27749,29548,2285,1097,9256,25863,25976,
30051,25972,2297,935,1097,9268,27396,31077,63,437,4598,1826,4662,
494,6,4246,494,8,3188,1826,4659,230,905,4091,1097,4103,1097,
9284,27395,31077,3647,4273,4246,4639,1826,4667,1097,9328,25860,
26989,116,3647,4266,4246,4639,1097,9348,25346,114,494,13,4678,
494,10,4678,1097,9366,26379,29797,25389,29301,25970,29806,4306,
4246,1097,9386,29451,29797,25389,29301,25970,29806,4306,4251,
1097,8908,27652,29537,116,4700,4246,1097,9406,28676,25449,107,
1403,1172,272,1097,9440,11010,33,2297,4472,272,1172,674,299,
1097,9456,27654,26739,26217,116,4479,1826,4752,236,674,4612,
674,1799,4743,1097,9476,25346,64,761,4246,674,1622,1826,4767,
494,8,2447,1799,4770,494,255,3188,1097,9506,25346,33,674,494,
255,3188,761,494,8,4743,2627,674,4472,761,4246,674,1622,2018,
494,255,2891,935,815,2891,1016,3188,2891,674,4251,1097,9542,
27907,30817,4510,2159,1826,4812,4465,1799,4813,905,1097,9604,
27907,28265,4510,2225,1826,4824,4465,1799,4825,905,1097,9628,
29449,30063,25458,11621,25705,575,36,4246,1097,9652,12802,33,
4472,4251,4625,4251,1097,9672,12802,64,761,4625,4246,674,4246,
1097,9688,12835,29246,1016,674,935,674,935,935,1097,9706,12835,
15986,1016,1016,674,1016,674,935,1097,9726,29699,28789,575,38,
1097,9746,29446,30063,25458,101,4876,4847,1097,9758,24839,26988,
28263,25701,761,1622,4522,4097,3188,1172,1097,9774,24837,26988,
28263,4352,4892,494,38,4251,1097,9798,24837,27756,29807,4892,
494,38,4731,1097,9818,11265,4903,4352,4251,4618,4913,1097,9836,
25349,30063,29806,761,242,674,4756,1097,9852,11015,29811,26994,
26478,4097,815,4817,4487,815,1172,4487,4487,1127,1097,9870,29700,
28793,101,761,1826,4964,674,4930,4678,674,236,1799,4954,4503,
1097,9900,25349,28525,25974,935,1799,4981,935,761,4756,1211,
4774,242,1016,242,1487,4973,4503,1097,9932,26116,27753,108,674,
935,674,1799,4997,4510,4774,242,1487,4994,4503,1097,9970,25861,
24946,25971,4091,4989,1097,9426,25603,9327,1016,1016,4612,761,
4930,1172,4892,2297,935,674,935,1097,10016,10243,10532,5011,1097,
10046,11778,36,5011,4930,4954,1097,10002,29445,24944,25955,4413,
4678,1097,10070,25349,29793,26723,1403,935,575,30,4246,935,1295,
575,30,4251,4639,1016,575,30,4251,1289,4091,1097,10084,29701,
29288,30575,4479,1826,5085,575,30,4246,1361,1016,575,30,4251,
1016,674,935,1472,905,1016,1097,10128,24837,28514,29810,4103,5068,
1097,10056,10247,25185,29295,10612,5011,674,1826,5105,4930,4954,
5090,905,1097,10186,25605,28773,26740,494,96,4246,1403,1127,236,
1097,10214,16134,25956,29808,104,5111,4539,1826,5130,494,-4,5068,
1097,10172,29955,11117,4510,1172,935,1211,4091,4539,935,4510,
3188,2087,1016,2627,935,2627,2087,1016,3188,248,242,1016,674,
1097,10262,25607,25966,24935,25972,248,935,248,4097,5134,1016,
1172,1097,10312,25602,43,935,674,935,5134,1016,1172,1016,1172,
1097,10338,29955,10861,4091,674,494,15,935,761,5134,935,935,761,
5134,1016,1172,1016,1826,5205,935,815,5134,1016,1172,1487,5189,
4487,905,1097,10362,10753,5184,905,1097,10420,29958,12141,28525,
100,4479,2018,1826,5227,494,-10,5068,4510,4557,1826,5270,4592,
494,15,935,935,761,5134,935,935,761,5134,1016,1172,761,1016,
1211,674,935,5134,1016,4522,674,4522,1172,1826,5263,935,905,
242,1016,1799,5264,905,1016,1487,5235,905,674,1097,4503,905,
4103,761,1097,10430,27909,27951,25711,4598,761,935,1826,5288,
4592,935,5161,1016,935,4598,1826,5294,1211,1172,1016,5220,1016,
1826,5303,674,4592,674,1097,1097,10550,12036,28525,100,815,2087,
674,5279,1097,10608,27907,25711,5308,905,1097,10626,12033,5308,
4465,1097,10236,10246,28005,29801,41,404,1097,10638,25860,26723,
111,4281,4246,4639,1097,10648,29699,28769,761,5335,815,4774,242,
1097,10678,27396,24948,112,761,761,494,13,4533,935,494,10,4533,
1016,3188,1826,5397,761,494,8,4533,935,494,127,4533,1016,3188,
1826,5380,4413,5342,1097,935,815,1211,2159,761,1826,5394,494,8,
761,5335,4413,5335,5335,1016,1172,1097,905,4465,761,1097,10662,
24838,25443,28773,116,815,1172,815,4510,4533,1826,5428,4667,761,
4413,1127,494,95,4557,1826,5425,5342,1799,5426,5352,1799,5409,
905,815,1127,1097,10802,29699,25193,4884,905,1097,10864,28933,
25973,31090,5435,494,256,5406,4876,4251,905,4091,4407,4251,1097,
10876,11529,29300,26977,26988,26478,935,1799,5473,4413,815,1211,
1172,4756,2159,1826,5473,1016,242,1097,1487,5462,4091,1097,10696,
27652,28527,107,674,935,4487,4487,761,1826,5508,815,4756,1211,
1127,1211,4413,4527,494,4,4724,4639,1826,5505,1289,4487,905,1097,
4940,1799,5485,1289,4487,905,1097,10954,29959,28014,29793,26723,
1826,5521,1976,1097,4522,1097,11024,27909,29793,26723,5517,248,
1097,10906,28677,29281,25971,935,5435,4407,4246,1172,4876,4246,
4407,4246,1127,1211,935,815,1016,674,935,935,1211,494,11034,
5481,4510,1016,494,11054,5481,674,1016,1127,935,1127,1016,242,
4407,4731,1016,4413,4527,1826,5575,5459,4091,4805,1097,11046,
25094,28257,25966,114,935,761,1976,1826,5593,1211,4678,236,
1799,5584,905,1289,1097,11060,26628,27759,100,4103,4372,4731,
4372,4246,4774,1097,11192,8962,62,4503,4372,4246,4322,494,896,
1172,815,1127,1097,11156,25863,29816,24946,29795,761,935,5220,
1016,674,935,5220,1016,4487,1097,11240,25605,26473,29801,494,
9,815,2159,494,7,3188,1172,494,48,1172,1097,11214,8961,494,
2,5123,4091,4360,4246,5625,5639,5600,1097,11302,8962,115,5653,
4510,2627,2018,1826,5666,1097,11326,15362,35,4322,494,896,1172,
4372,4251,1097,11346,29444,26473,110,2087,1826,5693,494,45,5600,
1097,11366,29955,29230,935,4091,5676,5666,5610,1016,815,1127,
4413,5583,4954,1097,11388,29954,46,4091,5676,5666,5610,5039,
4954,1097,11270,10243,10542,4604,4360,4246,3491,4479,1826,5730,
5722,5639,4678,1097,11418,11777,5039,761,2087,1826,5743,494,45,
4678,5722,1097,11466,15879,30062,25197,29285,4510,935,935,905,
4756,4360,4246,935,494,48,1127,494,9,815,2159,1826,5775,494,
7,1127,761,494,10,2159,2627,761,1016,4557,2018,1826,5785,905,
1016,1016,1097,674,4360,4246,5184,905,4487,4360,4246,5184,5172,
1016,1016,4940,761,2018,1826,5750,1097,11490,28167,28021,25954,
16242,4103,4366,4251,4360,4246,935,815,4756,494,45,4527,761,
935,1826,5824,4940,815,4756,494,36,4527,1826,5833,4427,4940,935,
935,4091,761,1016,1016,5750,761,1826,5868,815,4756,494,46,4533,
1826,5860,4487,905,4487,1016,4503,4091,1016,4360,4251,1097,236,
4366,4251,242,4366,4246,1799,5839,4503,1016,1826,5873,5161,1016,
4360,4251,4103,1097,11606,11778,115,5111,935,1799,5888,1211,4724,
5735,1487,5885,1097,11756,25351,28015,24944,25970,4487,815,1127,
4479,1826,5907,935,4503,1016,4465,1097,935,1799,5922,4930,4487,
4930,4487,1127,4479,1826,5922,1289,4465,4465,1097,1487,5910,
4503,4091,1097,11782,28163,24934,4625,1097,11854,25347,24934,
5930,761,4756,494,31,3188,1172,4625,4618,4592,3188,1097,11438,
10248,25971,29281,26723,41,674,935,761,761,1826,5989,761,5930,
4930,494,159,3188,1211,4930,5896,2018,1826,5984,1289,761,5930,
494,64,674,4246,3188,4522,4097,2627,4592,1097,4465,761,4246,
1799,5956,1289,4503,4091,1097,11894,10246,26982,25710,41,935,
4345,761,4246,1826,6022,761,4246,4246,1211,674,5953,4479,1826,
6019,935,4487,905,1016,1289,1097,4625,1799,6000,905,4091,1016,
4091,1097,11864,29455,24933,25458,11624,28535,25714,26988,29811,
5953,4487,905,1097,12054,26116,28265,100,5998,4487,905,1097,
11986,10249,26988,25972,24946,10604,4379,4246,1826,6062,494,494,
4920,4920,1097,12080,27719,29801,29285,27745,4290,4246,4639,1097,
12126,25352,28015,26992,25964,44,2297,4903,4920,1097,12096,16134,
28518,28277,100,1826,6090,1097,5039,4930,4954,494,63,4678,4686,
494,-13,5068,1097,12144,26889,29806,29285,29296,29797,6044,4479,
1826,6139,4379,4246,1826,6124,1976,1826,6121,5935,4639,1097,
5935,6078,1097,905,761,5930,4756,494,32,3188,1826,6136,494,
-14,5068,5935,4639,1097,761,935,4930,5808,1826,6162,1289,4366,
4246,2087,1826,6154,905,1799,6160,4379,4246,1826,6159,674,6068,
6068,1097,1016,4091,6087,1097,12202,26377,29797,28461,25714,29285,
4345,4091,935,761,4246,1211,2891,1826,6184,4625,1799,6175,1289,
761,4618,1127,674,4345,1127,2297,761,935,236,4598,1826,6201,
494,-80,5068,935,1799,6209,761,4246,674,4618,1127,1487,6204,
4246,1016,1097,0,29449,29797,28461,25714,29285,761,4103,4527,
1826,6230,905,4315,4097,6220,1097,761,4337,2225,1826,6238,494,
-73,5068,4345,674,935,1799,6246,4472,4251,4625,1487,6243,4091,
674,4251,1097,12428,26126,29295,26740,30509,29295,27748,29545,
116,494,64,1097,12504,29446,29561,25972,109,494,70,1097,12528,
26117,29295,26740,4315,6261,494,2,6220,1097,12544,28420,27758,
121,4103,6220,1097,12164,11779,25705,5930,4930,494,31,3188,4954,
5039,1097,12564,30469,29295,29540,4686,6172,4479,1826,6331,674,
4246,4479,1826,6328,761,5930,4756,494,128,3188,2018,1826,6325,
761,6292,4246,1799,6311,236,1799,6306,1097,12332,25611,26213,
28265,29801,28521,29550,4345,4246,4710,1097,12664,30468,29295,100,
5534,4352,761,935,4510,4251,242,674,4970,1016,1097,12578,16135,
28277,29033,25973,761,4700,5953,2018,1826,6370,1097,5039,4503,
494,76,4246,6292,5031,29193,25701,26213,28265,25701,4686,1097,
12716,16132,30062,108,761,4756,1826,6393,1097,494,-16,5068,1097,
12768,16132,25964,110,761,4756,494,31,2225,1826,6411,494,-19,
5068,1097,12686,25348,24936,114,4413,6347,6388,4930,905,4756,
1097,12824,23366,26723,29281,93,6416,494,494,4920,4920,1097,
12846,15201,494,-17730,4533,1826,6444,494,-22,5068,494,1097,
4920,4450,4479,1826,6454,4700,4251,1097,1097,12868,14849,4903,
4352,761,494,76,4251,4717,4920,4413,6347,6388,6401,6363,4930,
1172,494,38,4251,4903,494,-17730,4444,1097,12910,14855,28526,
24942,25965,4352,4091,494,-17730,4444,1097,12960,25189,26469,
28265,4903,4352,1097,12982,30053,29806,27753,494,1826,4920,2297,
4920,1097,12996,24933,24935,28265,494,1799,4920,2297,4920,1097,
13016,26978,102,494,1826,4920,4352,4091,4920,1097,13036,29796,
25960,110,4352,2297,674,4251,1097,13056,30565,26984,25964,6521,
1097,13074,29286,28773,24933,116,674,6512,6532,1097,13086,25956,
29548,101,494,1799,4920,4352,4091,4920,674,6532,1097,13104,26211,
29295,494,935,4920,4352,1097,13130,24931,29798,905,494,1799,4920,
4352,4091,4920,4903,4352,674,1097,13146,28260,30821,116,494,1487,
4920,2297,4920,1097,13174,10049,4413,6347,6044,6087,5935,6068,
1097,13194,25383,28015,26992,25964,1016,761,272,4920,242,935,
1097,13212,29287,25445,29301,25971,494,76,4246,5935,6078,1097,
12794,29702,26479,27751,101,4472,4246,2891,674,4251,1097,13258,
26628,25705,101,4413,6347,6044,6087,5930,494,128,674,6634,1097,
13280,10245,24950,10610,1016,4612,1097,13308,10247,28515,29550,
10612,1016,272,1097,13322,10248,24941,27506,29285,41,1016,4612,
761,4246,494,38,4251,4625,4246,4700,4251,1097,13236,25350,25970,
29793,101,6457,905,4450,6611,6658,4700,4251,1097,13374,30216,
29281,24937,27746,101,6692,4091,4920,1097,13400,25352,28271,29811,
28257,116,6692,4618,4592,4913,6611,6666,4920,1097,13420,15909,
28514,31076,4625,1097,13338,10246,28516,29541,41,1016,1016,4612,
674,935,1097,13460,10246,28515,28781,41,1016,494,76,4246,5935,
4251,1097,13448,25701,25967,15987,6611,6746,6611,6735,1097,13506,
27910,29281,25963,114,4717,4352,6692,4618,4592,4913,6611,6675,
4920,4920,1097,13524,29283,8560,6611,1361,1097,13556,29283,16496,
6611,1295,1097,13568,15970,114,6611,935,1097,13580,29282,62,6611,
1016,1097,13592,29282,64,6611,1211,1097,13604,29285,29284,28783,
6611,1289,1097,13616,25956,27000,116,6611,1097,1097,13630,11874,
34,6611,5031,494,34,6347,4930,1172,494,38,4251,4903,1097,13644,
9314,34,6611,5026,494,34,6347,4930,1172,494,38,4251,4903,1097,
13674,24934,28514,29810,34,6611,5098,494,34,6347,4930,1172,494,
38,4251,4903,1097,13704,10305,494,41,5534,4503,1097,13738,11842,
40,494,41,5534,4954,1097,13752,10561,1097,13768,23617,5435,4246,
4407,4251,1097,13774,28744,29551,28788,28271,101,4413,6347,6044,
6087,5935,6078,1097,13788,26889,28013,25701,24937,25972,4717,
5930,4246,494,64,2627,4717,5930,4251,1097,13814,29443,25957,4413,
6347,6044,6087,4686,761,4246,494,1097,4533,1826,6951,761,4246,
5735,4625,4352,815,2159,1826,6949,905,1097,1799,6931,4246,5712,
1097,13846,25604,28021,112,4892,4479,1826,6972,674,761,4246,
5735,4625,674,4618,1127,1799,6959,905,1097,13482,25349,29547,
28021,4892,761,494,-16162,1127,935,4479,1826,6999,674,761,4246,
1016,1172,935,4625,674,4618,1127,1799,6984,905,1016,1097,13908,
25607,26213,28265,25701,4413,6347,6044,4465,4522,1097,14004,23366,
26740,28261,93,1097,14026,23366,27749,25971,93,4413,6347,761,
4756,1826,7047,6044,905,5935,761,494,14048,4527,674,494,14036,
4527,2627,1826,7045,1097,1799,7024,5442,1799,7024,1097,14038,
23364,26217,93,1826,7058,1097,7024,1097,14102,27906,115,935,
3647,4389,4246,935,1487,7068,1487,7064,1097,14120,25092,27749,
108,494,7,4678,1097,13948,25347,26995,494,27,4678,494,91,4678,
1097,14146,28676,26465,101,7084,5031,12802,74,7084,5031,12548,
12603,72,1097,14182,24837,11636,31096,4360,4246,4437,935,7084,
4091,5697,5031,15105,4091,5697,5031,18433,1016,4360,4251,1097,
14210,25093,25135,26229,494,1024,1097,14252,25093,28524,27491,
4097,5123,761,4395,4251,494,10,4743,3647,1097,14266,26117,30060,
26739,1097,14294,29958,25712,29793,101,4103,494,74,4251,1097,
14304,25093,24940,27502,4413,4989,1097,14324,27652,29545,116,
7095,4686,761,4401,4251,7137,494,15,935,494,15,1211,1127,494,
3,5697,5039,494,63,935,4930,4678,1487,7193,4686,1487,7182,905,
1097,14338,26377,29797,26925,28782,29813,4884,4407,4246,4832,
4258,4246,1097,14404,29449,29797,26925,28782,29813,4258,4251,
575,36,4251,4407,4251,4876,4839,1097,14162,28418,107,4379,4246,
2018,1826,7243,5031,8195,27503,4686,1097,14462,25860,24950,108,
4413,6347,761,4756,1826,7259,6107,4091,5123,1799,7248,905,4258,
4246,4639,1097,14430,25864,24950,30060,29793,101,7208,4856,4856,
935,4091,4103,494,8486,7221,494,14496,5046,1016,4866,4866,7221,
5068,1097,14488,27652,28265,101,494,6,4743,674,7137,1172,494,
64,1097,14576,27656,24943,27748,28265,101,7292,7270,1097,14528,
27652,24943,100,4091,494,15,935,4510,4856,7307,4866,242,1487,
7318,4503,1097,12600,25862,28518,29810,104,494,263,1097,14602,
26884,26222,111,4686,5031,20501,28530,25962,29795,8250,18021,
29295,26740,30240,11825,8247,4686,5031,16667,29813,28520,14962,
8224,26962,26723,29281,8292,24906,25965,8307,28488,25975,4686,
5031,17694,24941,27753,8250,8224,28520,25975,29230,27182,14382,
16441,28007,26977,11884,28515,109,4686,5031,21033,28773,14959,
8224,8224,29800,28788,14963,12079,26983,26740,25205,25390,28015,
26671,30575,29285,12138,30067,27746,29029,4686,5031,19494,25449,
28261,25971,8250,26708,8293,28245,26988,25955,29550,8293,8239,
30032,27746,25449,17440,28015,26977,110,4686,1097,14670,29705,
29537,11627,28265,29801,494,40,4246,674,494,40,4251,4322,2297,
575,0,4251,494,460,2297,575,2,4251,4322,494,256,1172,2297,575,
6,4251,4322,494,512,1172,2297,575,8,4251,4091,575,4,4251,4437,
494,9292,4273,4251,494,10658,4281,4251,494,6,4246,1622,1826,7496,
494,8290,4281,4251,494,10658,4266,4251,494,14468,4258,4251,494,
12108,4290,4251,4091,4407,4251,4103,4366,4251,4322,494,512,1172,
4091,4876,4839,4450,494,40,4251,1097,14866,26883,26990,494,40,
4246,7439,1097,14620,28932,26997,116,5442,494,14496,5046,4479,
1826,7558,761,5039,5735,494,63,4678,4686,4103,4527,1826,7557,
230,7529,1799,7538,1097,15052,10246,28515,25708,41,6286,6276,
6339,7529,494,6,4246,494,4,3188,1826,7579,7339,494,6,4246,494,
2,3188,1826,7619,494,8,4246,4612,761,4352,674,1127,6978,494,
44,4246,4533,1826,7610,5031,25354,29547,28021,26144,26977,108,
230,494,6,4246,494,2,2891,494,6,4251,7538,1097,15122,29701,
29537,14955,6692,4352,494,1024,4913,2297,7439,1097,15242,24840,
29795,30313,29793,101,761,7439,761,935,674,2297,674,494,2,1172,
4251,1016,4322,4246,935,761,2297,4322,4251,1016,674,4251,1097,
15266,30468,26977,116,3647,761,4246,1826,7666,4091,674,4251,1097,
15324,29446,26473,24942,108,4322,674,4251,1097,15350,29446,28265,
27751,101,4097,494,80,4251,1097,15368,27909,27765,26996,4091,
494,80,4251,1097,15388,29444,28261,100,4322,815,494,32,1172,
3647,761,4246,2018,1826,7712,4251,494,34,1172,4251,1097,15406,
29191,25445,26981,25974,3647,575,32,4246,1826,7729,575,34,4246,
575,32,4246,4091,575,32,4251,1097,15068,25862,26980,28532,114,
494,66,4097,6220,1097,0,28929,6286,6276,1097,15512,16129,4401,
4246,5735,1097,15522,27649,4401,4246,7173,1097,15534,25857,7758,
4401,4246,7314,7751,1097,15546,26882,97,494,2,5123,494,6,4743,
1172,4401,4246,7137,1172,5435,4407,4246,1172,674,4884,4465,4407,
4246,1127,4970,5435,4246,4407,4251,7157,7769,1097,15562,26881,
4091,674,7784,1097,15626,30465,6304,1097,15638,29441,7157,7151,
1097,15646,28161,4097,4401,4731,7769,1097,15656,28673,4103,4401,
4731,7769,1097,15670,29185,4401,4251,7769,1097,15684,30721,4401,
4246,7137,7130,7166,7769,1097,15696,25601,4097,5123,935,4401,
4246,7137,1016,494,6,4743,1172,494,64,7166,7769,1097,15492,
25348,27759,100,494,72,4612,4246,4639,1097 }; while(p>=0){int
a=m[p++],b=m[p++],c=m[p++];a<0?m[b]=getchar():b<0?putchar(m[a]):(m[b]-=m[a])
<=0?p=c:0;}}
