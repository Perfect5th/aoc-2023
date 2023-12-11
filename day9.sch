(define EXAMPLE
  '((0 3 6 9 12 15)
    (1 3 6 10 15 21)
    (10 13 16 21 30 45)))

;; Produces #t if all values in seq are 0, else #f.
(define all-zero?
  (lambda (seq)
    (cond
     [(null? seq) #t]
     [(zero? (car seq)) (all-zero? (cdr seq))]
     [else #f])))

;; Produces a new list in acc, made from the differences between s and the prev value.
(define diffs
  (lambda (acc s)
    (let ([prev (car acc)]
          [new-list (cdr acc)])
      (cons s (cons (- s prev) new-list)))))

;; Comes up with a new value for the final list in acc by adding the new final value of each list
;; to the end of the next one.
(define predict-nexts
  (lambda (seq)
    (let f ([s seq] [next 0])
      (if (null? s) next
          (let ([rev (reverse (car s))])
            (f (cdr s) (+ (car rev) next)))))))

;; Reduces seq to zeroes, producing each step along the way.
(define reduce
  (lambda (seq)
    (let f ([s seq] [acc (cons seq '())])
      (if (all-zero? s)
          (cdr acc)
          (let ([next (reverse (cdr (fold-left diffs (cons (car s) '()) (cdr s))))])
            (f next (cons next acc)))))))

;; Reduces each seq to zeroes, then sums the next predicted values for the original sequence.
(define solve1
  (lambda (seqs)
    (apply + 0 (map predict-nexts (map reduce seqs)))))

;; Comes up with a new value for the final list in acc by adding the new first value of each
;; list to the start of the next one.
(define predict-prevs
  (lambda (seq)
    (let f ([s seq] [prev 0])
      (if (null? s) prev
          (f (cdr s) (- (caar s) prev))))))

;; Reduces each seq to zeroes, then sums the prev predicted values for the original sequence.
(define solve2
  (lambda (seqs)
    (apply + 0 (map predict-prevs (map reduce seqs)))))

(define INPUT
  '((25 43 75 138 255 455 773 1250 1933 2875 4135 5778 7875 10503 13745 17690 22433 28075 34723 42490 51495)
    (15 22 50 114 229 410 672 1030 1499 2094 2830 3722 4785 6034 7484 9150 11047 13190 15594 18274 21245)
    (25 37 45 57 103 257 670 1617 3564 7271 13964 25633 45561 79327 136917 237555 421062 774117 1488793 2987794 6182124)
    (3 17 54 120 230 425 791 1489 2829 5466 10872 22355 47064 99648 208537 426194 845159 1622279 3014202 5428018 9491866)
    (25 47 77 111 155 235 402 724 1251 1932 2462 2054 -809 -8433 -23232 -45063 -64008 -43580 115154 646196 2060426)
    (9 15 39 95 208 426 851 1716 3546 7461 15730 32813 67431 136877 276189 557604 1131039 2306127 4713741 9615035 19476706)
    (1 -6 -8 15 106 342 853 1867 3810 7496 14449 27435 51408 95395 176521 328632 620115 1188930 2308050 4502051 8746226)
    (12 17 22 27 32 37 42 47 52 57 62 67 72 77 82 87 92 97 102 107 112)
    (17 18 18 15 3 -16 -9 122 585 1754 4236 8949 17211 30840 52265 84648 132017 199410 293030 420411 590595)
    (-4 10 52 139 306 624 1231 2386 4556 8546 15682 28057 48850 82728 136341 218920 342988 525194 787280 1157191 1670338)
    (-1 9 22 33 37 29 4 -43 -117 -223 -366 -551 -783 -1067 -1408 -1811 -2281 -2823 -3442 -4143 -4931)
    (12 25 38 51 64 77 90 103 116 129 142 155 168 181 194 207 220 233 246 259 272)
    (4 13 46 121 263 512 939 1685 3051 5677 10848 20953 40109 74975 135876 238622 407970 684713 1140118 1904165 3218119)
    (9 5 4 10 27 59 110 184 285 417 584 790 1039 1335 1682 2084 2545 3069 3660 4322 5059)
    (11 17 31 60 115 216 406 790 1625 3499 7665 16678 35706 75431 158678 335480 716439 1544189 3341367 7207300 15395413)
    (1 3 12 42 111 241 458 792 1277 1951 2856 4038 5547 7437 9766 12596 15993 20027 24772 30306 36711)
    (8 14 20 26 32 38 44 50 56 62 68 74 80 86 92 98 104 110 116 122 128)
    (2 17 45 90 167 309 573 1045 1844 3125 5081 7944 11985 17513 24873 34443 46630 61865 80597 103286 130395)
    (3 5 8 25 83 230 541 1117 2066 3459 5278 7432 10045 14499 26326 62366 167432 451676 1172223 2907858 6925638)
    (15 31 53 81 115 155 201 253 311 375 445 521 603 691 785 885 991 1103 1221 1345 1475)
    (24 43 79 142 248 434 777 1425 2670 5120 10065 20208 41107 84069 172095 352295 721984 1483393 3057187 6314066 13037102)
    (13 27 44 74 134 244 434 784 1532 3314 7658 17954 41277 91663 195742 402030 795687 1521173 2815992 5059618 8842760)
    (-5 6 26 65 149 320 644 1246 2412 4831 10091 21583 45992 95545 191119 366158 671073 1177364 1980064 3196213 4955871)
    (14 20 31 51 92 179 352 674 1274 2485 5189 11576 26705 61603 139298 306401 655034 1363686 2771955 5514593 10757022)
    (8 20 31 43 76 193 546 1449 3498 7798 16438 33501 67151 133785 266032 527840 1042692 2046618 3989373 7731849 14939606)
    (-2 13 44 108 239 501 1024 2092 4340 9158 19454 40995 84625 169752 329602 618857 1124426 1980243 3387144 5639046 9156835)
    (15 23 45 84 135 188 234 271 307 357 431 510 507 210 -796 -3231 -8249 -17597 -33807 -60424 -102273)
    (19 24 29 34 39 44 49 54 59 64 69 74 79 84 89 94 99 104 109 114 119)
    (16 22 33 66 144 295 567 1067 2029 3920 7606 14625 27670 51529 95095 175938 330930 640746 1285035 2662828 5646480)
    (18 20 33 73 155 290 482 725 1000 1272 1487 1569 1417 902 -136 -1891 -4594 -8516 -13971 -21319 -30969)
    (9 23 52 101 177 296 489 820 1443 2739 5588 11845 25103 51840 103061 196560 359941 634551 1080492 1782893 2859637)
    (23 37 52 75 129 267 593 1301 2757 5675 11489 23130 46640 94494 192307 391989 796663 1606149 3197006 6258585 12016967)
    (5 25 58 104 163 235 320 418 529 653 790 940 1103 1279 1468 1670 1885 2113 2354 2608 2875)
    (7 22 47 94 178 326 611 1226 2620 5742 12503 26703 55923 115304 234789 472363 937173 1828238 3496871 6544044 11967856)
    (-2 6 24 52 90 138 196 264 342 430 528 636 754 882 1020 1168 1326 1494 1672 1860 2058)
    (15 36 70 118 185 296 529 1079 2385 5385 12023 26230 55766 115612 234193 464886 907519 1748686 3336864 6320194 11893675)
    (18 42 88 181 356 667 1211 2170 3871 6868 12079 21090 36899 65655 120396 228459 445184 877830 1725338 3341795 6334262)
    (10 26 46 67 86 100 106 101 82 46 -10 -89 -194 -328 -494 -695 -934 -1214 -1538 -1909 -2330)
    (1 1 13 54 148 337 710 1457 2948 5823 11054 19900 33617 52703 75349 94627 93771 38693 -133381 -530574 -1330094)
    (7 18 39 90 201 408 744 1225 1831 2482 3009 3120 2361 72 -4662 -13065 -26729 -47678 -78437 -122106 -182439)
    (7 0 1 27 100 247 500 896 1477 2290 3387 4825 6666 8977 11830 15302 19475 24436 30277 37095 44992)
    (0 0 1 1 -1 -2 7 42 127 297 606 1144 2068 3653 6370 10999 18786 31654 52479 85443 136477)
    (24 34 41 55 97 200 413 808 1490 2610 4381 7097 11155 17080 25553 37442 53836 76082 105825 145051 196133)
    (6 4 19 65 166 375 815 1766 3846 8378 18116 38645 80997 166359 334236 656234 1259234 2365352 4364329 7951905 14404284)
    (11 12 16 29 58 106 167 221 229 128 -174 -803 -1924 -3746 -6527 -10579 -16273 -24044 -34396 -47907 -65234)
    (14 27 63 147 328 703 1456 2923 5710 10912 20507 38030 69668 125958 224316 392676 674574 1136073 1874991 3032965 4810960)
    (13 25 30 25 4 -32 -43 124 895 3293 9553 24231 56132 121576 249820 491898 934777 1723609 3096048 5434165 9341508)
    (8 16 42 96 184 318 544 1000 2022 4316 9216 19077 37959 73044 137896 260121 498946 984006 1997384 4140260 8659152)
    (4 20 57 125 234 394 615 907 1280 1744 2309 2985 3782 4710 5779 6999 8380 9932 11665 13589 15714)
    (18 36 76 154 300 567 1040 1845 3158 5214 8316 12844 19264 28137 40128 56015 76698 103208 136716 178542 230164)
    (6 17 34 72 167 394 893 1903 3804 7167 12812 21874 35877 56816 87247 130385 190210 271581 380358 523532 709363)
    (16 17 14 0 -26 -49 -17 202 891 2575 6106 12709 23985 41967 69541 111931 180570 301613 532678 993222 1916376)
    (24 36 60 106 189 342 632 1188 2268 4418 8811 17901 36591 74202 147651 286411 540048 989424 1763038 3060468 5185497)
    (14 21 28 35 42 49 56 63 70 77 84 91 98 105 112 119 126 133 140 147 154)
    (8 14 15 5 -26 -94 -224 -454 -840 -1462 -2431 -3897 -6058 -9170 -13558 -19628 -27880 -38922 -53485 -72439 -96810)
    (9 8 8 30 120 366 922 2039 4103 7680 13568 22856 36990 57846 87810 129865 187685 265736 369384 505010 680132)
    (2 3 14 59 175 416 867 1692 3257 6397 12943 26699 55168 112478 224162 434708 819124 1500167 2673372 4642595 7869461)
    (14 22 53 124 254 464 777 1218 1814 2594 3589 4832 6358 8204 10409 13014 16062 19598 23669 28324 33614)
    (4 15 31 65 143 306 624 1245 2523 5296 11418 24688 52364 107501 212409 403590 738582 1305213 2233849 3713307 6011197)
    (20 29 45 77 136 238 423 800 1644 3609 8197 18762 42564 94764 205819 434557 890356 1769397 3411997 6390651 11641728)
    (2 -5 -12 -3 49 194 530 1239 2632 5201 9676 17085 28815 46672 72938 110423 162510 233191 327092 449485 606285)
    (27 48 74 110 170 277 463 769 1245 1950 2952 4328 6164 8555 11605 15427 20143 25884 32790 41010 50702)
    (15 28 57 105 168 238 317 459 883 2244 6224 16734 42234 100024 223888 477248 975079 1919332 3654603 6753375 12143460)
    (16 34 66 112 172 246 334 436 552 682 826 984 1156 1342 1542 1756 1984 2226 2482 2752 3036)
    (10 12 26 68 166 373 788 1585 3050 5626 9966 16994 27974 44587 69016 104039 153130 220568 311554 432336 590342)
    (9 33 82 178 362 705 1335 2492 4623 8529 15576 27982 49192 84353 140901 229272 363749 563457 853518 1266378 1843318)
    (10 8 21 71 194 447 915 1718 3018 5026 8009 12297 18290 26465 37383 51696 70154 93612 123037 159515 204258)
    (2 17 51 109 205 370 663 1197 2198 4121 7853 15039 28573 53302 96997 171651 295170 493529 803471 1275833 1979589)
    (12 20 44 98 202 398 780 1550 3128 6369 12986 26380 53305 107260 215363 431941 864440 1720854 3393084 6597914 12607128)
    (21 51 110 218 400 684 1110 1765 2865 4914 8996 17324 34313 68700 137643 274332 541473 1054089 2016438 3779482 6927242)
    (18 32 46 60 74 88 102 116 130 144 158 172 186 200 214 228 242 256 270 284 298)
    (-8 -12 -10 0 23 86 280 848 2350 5938 13767 29549 59223 111662 199265 338185 547820 849040 1260436 1791654 2432615)
    (18 35 57 78 83 57 7 -3 196 939 2801 6684 13917 26369 46575 77875 124566 192067 287097 417866 594279)
    (6 9 15 24 43 93 223 543 1291 2952 6450 13437 26706 50758 92556 162502 275676 453379 725025 1130430 1722549)
    (11 12 20 55 160 420 998 2194 4525 8817 16309 28822 49187 82417 138630 239583 431979 812594 1572886 3074249 5969622)
    (13 20 31 46 58 58 47 59 217 882 3020 9022 24420 61383 145852 332290 734422 1589037 3386318 7129763 14840179)
    (-3 3 20 48 87 137 198 270 353 447 552 668 795 933 1082 1242 1413 1595 1788 1992 2207)
    (16 30 55 106 201 363 639 1146 2154 4216 8355 16318 30907 56397 99051 167742 274692 436338 674335 1016706 1499149)
    (18 37 74 140 247 401 590 765 826 655 298 511 4079 18621 62034 172324 424330 956761 2014010 4008334 7609119)
    (11 17 23 29 35 41 47 53 59 65 71 77 83 89 95 101 107 113 119 125 131)
    (21 34 52 89 171 342 674 1282 2364 4316 8023 15535 31590 67018 146244 323350 715118 1566195 3372779 7110296 14643524)
    (4 14 29 52 97 209 496 1173 2618 5440 10559 19298 33487 55579 88778 137179 205920 301346 431185 604736 833069)
    (17 23 38 66 104 143 181 269 628 1909 5729 15718 39484 92203 203086 427013 865693 1706967 3299762 6300778 11967458)
    (6 15 27 46 88 204 522 1314 3104 6852 14277 28419 54586 101887 185616 330825 577506 987893 1656495 2723580 4392948)
    (8 13 24 43 71 112 173 263 401 662 1333 3330 9154 24851 63700 152696 343334 728747 1469918 2834485 5252601)
    (15 28 54 115 244 488 911 1597 2653 4212 6436 9519 13690 19216 26405 35609 47227 61708 79554 101323 127632)
    (21 36 66 119 213 398 801 1708 3700 7863 16095 31536 59150 106491 184688 309687 503791 797542 1231992 1861413 2756499)
    (11 37 85 163 276 427 625 916 1462 2699 5608 12133 25777 52401 101242 186154 327061 551593 896855 1411255 2156290)
    (-8 3 34 100 228 459 847 1467 2465 4216 7711 15396 32890 72420 159609 346723 738050 1537353 3134140 6255932 12229220)
    (8 19 35 63 128 277 592 1224 2460 4835 9301 17465 31908 56597 97402 162730 264288 417987 644999 972979 1437464)
    (-6 -10 -20 -45 -104 -225 -441 -771 -1154 -1267 -94 4992 20051 58048 145352 333842 723414 1503576 3029356 5957273 11483438)
    (14 23 37 65 133 309 745 1746 3887 8218 16642 32656 62862 120056 229382 440113 847236 1629345 3112587 5874795 10909747)
    (5 -2 -10 -5 46 203 566 1294 2628 4921 8693 14759 24538 40772 69130 121701 224611 434984 879612 1844031 3975938)
    (3 21 52 98 164 258 391 577 833 1179 1638 2236 3002 3968 5169 6643 8431 10577 13128 16134 19648)
    (7 8 19 43 82 153 318 734 1746 4081 9271 20562 44795 96170 203655 425577 878602 1795615 3640864 7335774 14692157)
    (21 30 39 48 57 66 75 84 93 102 111 120 129 138 147 156 165 174 183 192 201)
    (-4 8 31 65 110 166 233 311 400 500 611 733 866 1010 1165 1331 1508 1696 1895 2105 2326)
    (18 41 91 193 391 753 1374 2377 3912 6153 9293 13537 19093 26161 34920 45513 58030 72489 88815 106817 126163)
    (-4 -13 -21 -20 7 104 372 1011 2374 5033 9857 18102 31513 52438 83954 130005 195552 286735 411047 577520 796923)
    (10 21 38 65 122 269 649 1560 3571 7704 15715 30537 57037 103464 184452 327378 585520 1064159 1969964 3700241 6997588)
    (11 25 53 95 151 221 305 403 515 641 781 935 1103 1285 1481 1691 1915 2153 2405 2671 2951)
    (19 27 37 67 155 366 799 1594 2939 5077 8313 13021 19651 28736 40899 56860 77443 103583 136333 176871 226507)
    (9 15 21 27 33 39 45 51 57 63 69 75 81 87 93 99 105 111 117 123 129)
    (3 2 4 8 26 103 342 932 2177 4524 8588 15172 25280 40121 61102 89808 127967 177398 239940 317360 411238)
    (0 7 22 49 97 192 400 861 1834 3753 7294 13453 23635 39754 64344 100681 152916 226219 326934 462745 642853)
    (11 5 1 12 65 214 560 1278 2651 5111 9287 16060 26625 42560 65902 99230 145755 209417 294989 408188 555793)
    (-3 -7 -1 22 71 172 404 980 2426 5953 14170 32348 70514 146728 291973 557166 1022875 1812401 3108953 5177706 8393585)
    (4 9 33 104 270 605 1221 2293 4106 7135 12171 20508 34208 56463 92075 148077 234520 365453 560125 844440 1252698)
    (3 13 32 65 140 331 798 1866 4176 8959 18520 37081 72229 137365 255798 467604 841455 1496343 2644958 4686847 8416272)
    (-4 9 37 80 138 211 299 402 520 653 801 964 1142 1335 1543 1766 2004 2257 2525 2808 3106)
    (5 0 -5 -5 5 30 75 145 245 380 555 775 1045 1370 1755 2205 2725 3320 3995 4755 5605)
    (25 43 78 151 294 549 981 1731 3161 6185 12948 28136 61426 132001 276787 565292 1125873 2191223 4175226 7799525 14296726)
    (9 23 54 104 171 247 318 383 521 1055 2904 8310 22339 55989 132571 300568 658990 1406445 2934993 6008892 12105512)
    (7 15 24 30 43 106 335 996 2636 6286 13755 28035 53838 98287 171784 289079 470565 743825 1145458 1723212 2538453)
    (0 -1 10 60 204 537 1214 2494 4829 9024 16499 29689 52623 91728 156909 262961 431374 692597 1088832 1677434 2534998)
    (17 40 84 169 335 649 1215 2194 3851 6665 11565 20397 36818 68040 128383 246740 482281 956726 1918286 3863249 7757949)
    (24 34 37 44 77 177 417 926 1945 3956 7950 15941 31926 63720 126631 251103 498806 996122 2002039 4041362 8157171)
    (10 7 20 65 159 334 677 1412 3045 6604 14025 28763 56755 107970 199046 358139 634506 1118355 1982836 3573131 6593957)
    (-5 -9 -13 -17 -21 -25 -29 -33 -37 -41 -45 -49 -53 -57 -61 -65 -69 -73 -77 -81 -85)
    (3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43)
    (5 9 28 84 209 461 953 1906 3755 7374 14556 29001 58243 117200 234373 462165 893353 1686439 3103444 5564706 9726413)
    (-3 0 6 27 91 255 623 1366 2741 5106 8928 14781 23331 35305 51441 72416 98749 130676 167994 209871 254619)
    (19 35 54 75 105 168 319 665 1404 2927 6101 12983 28438 63494 141816 311463 665149 1373687 2739636 5282855 9880339)
    (9 21 42 94 206 409 729 1184 1806 2730 4424 8196 17247 38823 88604 199621 440159 948983 2002842 4141990 8396292)
    (2 10 25 52 109 229 473 974 2037 4330 9220 19343 39571 78721 152826 292003 555888 1069234 2103267 4256230 8838981)
    (7 13 27 53 113 265 622 1380 2878 5733 11118 21281 40438 76213 141843 259416 464465 812301 1386533 2310293 3760759)
    (2 4 18 66 183 417 835 1536 2679 4547 7696 13317 24160 46931 98382 220159 513320 1214890 2860322 6614548 14921901)
    (20 42 89 187 375 705 1247 2114 3529 5965 10401 18760 34661 64809 121837 230509 441382 860016 1706553 3431097 6926055)
    (-6 5 41 118 257 497 927 1758 3479 7170 15084 31669 65309 131290 256992 491376 923096 1716263 3182439 5926360 11139249)
    (15 28 50 104 234 511 1047 2038 3883 7461 14703 29703 60831 124753 254153 512771 1025137 2036155 4031423 7980034 15819960)
    (10 6 9 29 78 167 305 517 915 1884 4505 11474 29080 71443 169472 389344 869428 1891498 4013213 8306107 16769254)
    (10 9 13 26 47 67 72 60 80 301 1119 3310 8237 18119 36370 68016 120198 202769 328993 516354 787483)
    (11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51)
    (22 49 104 198 350 603 1059 1958 3838 7824 16114 32786 65193 126523 240683 451628 840658 1556967 2868476 5240830 9450668)
    (16 39 79 153 287 511 853 1343 2046 3153 5181 9391 18683 39591 86823 193521 433828 970699 2153123 4705917 10086151)
    (5 16 35 58 78 83 57 -1 -19 264 1480 4880 12692 28852 61020 126369 267104 588917 1350257 3159230 7390503)
    (18 28 39 54 85 169 397 952 2164 4623 9465 19094 38871 80748 170523 361426 758214 1557966 3116451 6052430 11406697)
    (3 5 19 48 91 146 221 356 666 1430 3272 7505 16736 35857 73572 144631 272957 495859 869521 1475942 2431473)
    (30 41 45 51 93 250 668 1574 3282 6229 11158 19698 35791 68696 138673 286929 592006 1195521 2341043 4430925 8107113)
    (11 22 53 115 230 453 910 1857 3765 7436 14155 25883 45496 77075 126252 200617 310191 467970 690545 998803 1418714)
    (5 15 35 70 138 276 563 1187 2595 5777 12747 27296 56104 110310 207651 375293 653489 1100211 1796915 2855610 4427414)
    (13 15 18 28 59 132 275 532 1003 1974 4268 10067 24633 59605 138881 308521 652641 1317921 2550134 4747030 8532991)
    (19 31 39 37 30 50 187 653 1901 4825 11071 23493 46792 88380 159515 276757 463799 753731 1191799 1838725 2774658)
    (4 -4 -5 12 63 178 426 964 2108 4408 8686 15974 27291 43265 63797 88353 118140 162456 252973 471660 1000479)
    (8 26 56 102 170 269 420 688 1275 2754 6596 16260 39305 91285 202677 430899 880826 1739482 3334372 6231134 11396188)
    (11 23 35 47 59 71 83 95 107 119 131 143 155 167 179 191 203 215 227 239 251)
    (17 23 29 48 111 275 640 1382 2809 5447 10163 18332 32055 54435 89918 144706 227249 348823 524201 772424 1117679)
    (5 12 24 46 90 177 332 567 849 1058 963 297 -884 -1191 4419 29378 104397 293136 718466 1604688 3343062)
    (10 24 38 53 80 146 294 583 1116 2156 4430 9770 22312 50621 111465 236805 487438 980612 1946642 3844527 7595464)
    (4 -1 2 29 103 254 519 942 1574 2473 3704 5339 7457 10144 13493 17604 22584 28547 35614 43913 53579)
    (5 3 8 25 54 90 123 138 115 29 -150 -457 -932 -1620 -2571 -3840 -5487 -7577 -10180 -13371 -17230)
    (19 30 46 85 184 416 929 2025 4306 8935 18119 36059 70900 138785 272218 537010 1065858 2123285 4228117 8378734 16457241)
    (4 12 21 29 29 14 -11 -8 139 735 2539 7342 19120 46167 104805 225519 462686 909470 1719953 3141179 5558519)
    (12 17 33 83 215 524 1191 2554 5245 10458 20461 39545 75752 144023 272029 511254 958606 1799275 3392061 6437763 12305329)
    (10 26 62 133 273 562 1163 2376 4720 9059 16803 30264 53379 93309 164006 293873 541334 1024736 1976820 3839359 7420837)
    (26 38 50 62 74 86 98 110 122 134 146 158 170 182 194 206 218 230 242 254 266)
    (17 33 59 102 172 282 448 689 1027 1487 2097 2888 3894 5152 6702 8587 10853 13549 16727 20442 24752)
    (14 31 60 120 251 526 1063 2037 3692 6353 10438 16470 25089 37064 53305 74875 103002 139091 184736 241732 312087)
    (23 39 58 80 105 133 164 198 235 275 318 364 413 465 520 578 639 703 770 840 913)
    (21 45 93 181 329 569 959 1612 2762 4916 9199 18122 37251 78725 168410 359900 760925 1581649 3221326 6423446 12558361)
    (-5 -6 8 60 185 441 929 1831 3495 6640 12841 25616 52734 110904 234974 495463 1031149 2108352 4226908 8307194 16017545)
    (13 24 53 117 244 473 854 1448 2327 3574 5283 7559 10518 14287 19004 24818 31889 40388 50497 62409 76328)
    (23 40 64 104 189 396 894 2017 4403 9279 19055 38564 77656 156620 317410 646440 1319635 2688726 5441230 10887612 21461091)
    (15 35 59 84 104 107 73 -29 -252 -702 -1606 -3439 -7137 -14428 -28318 -53774 -98651 -174915 -300219 -499894 -809422)
    (0 7 22 50 96 165 262 392 560 771 1030 1342 1712 2145 2646 3220 3872 4607 5430 6346 7360)
    (2 12 37 84 178 378 809 1720 3572 7147 13653 24792 42771 70275 110468 167082 244466 346870 474873 614196 707420)
    (19 35 73 159 337 676 1293 2416 4518 8558 16368 31226 58654 107477 191174 329545 550709 893437 1409811 2168185 3256407)
    (9 10 18 54 150 354 744 1462 2789 5290 10064 19142 36094 66946 121586 215974 375689 641674 1079510 1794198 2953294)
    (-8 -9 1 44 170 476 1137 2452 4909 9283 16817 29640 51840 92218 171054 335896 696763 1501852 3296020 7246003 15797579)
    (27 37 57 108 228 494 1063 2249 4674 9561 19276 38271 74625 142426 265316 481736 852989 1476613 2510427 4218046 7056162)
    (0 4 9 15 22 30 39 49 60 72 85 99 114 130 147 165 184 204 225 247 270)
    (24 41 71 123 206 329 501 731 1028 1401 1859 2411 3066 3833 4721 5739 6896 8201 9663 11291 13094)
    (16 28 54 121 284 652 1429 2986 6003 11753 22654 43318 82532 157006 298461 566931 1075374 2034406 3832134 7174207 13325792)
    (9 2 -1 22 122 403 1062 2465 5288 10752 20979 39509 72072 127838 221613 377857 638023 1073610 1808547 3056140 5177882)
    (8 6 -4 -16 -3 97 412 1212 3075 7218 16131 34785 72949 149641 301604 599185 1175500 2278885 4365104 8255185 15396599)
    (-1 0 -2 -4 0 18 58 136 333 985 3169 9798 27930 73457 180383 418770 928688 1983079 4101909 8255993 16225829)
    (10 7 18 60 154 341 715 1481 3050 6188 12243 23483 43588 78348 136624 231626 382546 616549 971064 1496222 2257150)
    (-2 2 23 87 236 541 1124 2188 4054 7204 12329 20381 32628 50711 76702 113162 163198 230518 319483 435155 583340)
    (-4 -5 -9 -13 -5 34 136 384 1018 2657 6697 15957 35657 74824 148234 279010 502008 868135 1449755 2347351 3697623)
    (16 14 9 16 60 175 409 841 1620 3040 5669 10554 19528 35649 63805 111523 190024 315570 511153 808580 1251012)
    (-10 -17 -26 -37 -50 -65 -82 -101 -122 -145 -170 -197 -226 -257 -290 -325 -362 -401 -442 -485 -530)
    (-2 -3 2 27 101 292 744 1727 3700 7387 13866 24671 41907 68378 107728 164595 244778 355417 505186 704499 965729)
    (14 19 26 35 46 59 74 91 110 131 154 179 206 235 266 299 334 371 410 451 494)
    (7 12 28 64 131 242 412 658 999 1456 2052 2812 3763 4934 6356 8062 10087 12468 15244 18456 22147)
    (14 28 39 55 110 281 718 1708 3804 8057 16389 32139 60822 111236 197437 343264 594082 1047282 1927607 3762071 7762782)
    (5 25 56 98 154 234 364 613 1160 2436 5396 12008 26111 54967 112318 225021 448287 900835 1843624 3848550 8143064)
    (10 35 77 139 224 335 475 647 854 1099 1385 1715 2092 2519 2999 3535 4130 4787 5509 6299 7160)
    (7 14 17 19 45 153 454 1150 2598 5424 10768 20884 40634 81060 167480 356960 774538 1685999 3640496 7744350 16177032)
    (0 2 19 63 160 370 813 1711 3477 6920 13712 27414 55642 114484 237295 492021 1015343 2080463 4232825 8563395 17259507)
    (12 41 89 163 277 462 780 1342 2329 4016 6810 11357 18909 32508 60437 125397 287091 694318 1698939 4095063 9606004)
    (21 27 27 26 46 130 358 886 2028 4432 9463 20008 42069 87719 180272 362870 712127 1359001 2519699 4540166 7958576)
    (16 12 8 10 25 61 127 233 390 610 906 1292 1783 2395 3145 4051 5132 6408 7900 9630 11621)
    (4 20 49 89 144 249 517 1225 2979 7045 16029 35280 75762 159850 332827 685291 1396165 2814424 5613910 11087815 21714227)
    (6 22 55 114 213 377 658 1168 2136 3996 7513 13954 25311 44583 76124 126064 202810 317634 485355 725122 1061305)
    (1 3 7 17 57 186 511 1189 2411 4377 7310 11626 18478 31019 56865 112353 229233 464339 912461 1721971 3111595)
    (27 41 63 115 229 447 829 1486 2678 5049 10112 21147 44734 93211 188424 367222 689245 1247657 2183589 3705179 6112227)
    (22 38 68 121 204 333 552 971 1841 3684 7487 14956 28841 53475 96109 170733 308506 583836 1174568 2492081 5448060)
    (1 14 38 81 162 330 694 1461 2976 5757 10528 18293 30599 50369 84163 147660 277931 559352 1175870 2512485 5345816)
    (2 6 10 14 18 22 26 30 34 38 42 46 50 54 58 62 66 70 74 78 82)
    (10 23 57 123 244 483 982 2019 4099 8102 15522 28868 52407 93688 166813 299382 546656 1019041 1932855 3699937 7079518)))
