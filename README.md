# ccdtest
Cross-Correlation Differential Time ESTimator (ccdtest)

This program measures the relative times between similar signals on a single station (or multiple closely spaced stations - i.e. an array) for two different events.
We need an input file of the following format:  

```
WS  3.5    0.2 
WS  3.0    0.2 
WS  3.0    0.2 
WS  2.5    0.25
FS  0.8    2.2   4   2
FS  1.0    2.5   2   2
FS  1.2    2.8   4   2
FS  1.4    3.5   4   2
FS  1.8    4.0   4   2
FS  2.2    4.5   4   2
WF DPRK5_DPRK6_ILAR_example/DPRK6_IL01_IM_--_SHZ_.sac DPRK5_DPRK6_ILAR_example/DPRK5_IL01_IM_--_SHZ_.sac
```

Each line beginning **WS** is a **window specification** and is followed by two numbers:  
(a) the length of the window in seconds, and  
(b) the spacing between multiple occurrences of the window, in seconds.  

Each line beginning **FS** is a **filter specification** and is followed by:  
(a) low frequency (Hz)  
(b) high frequency (Hz)  
(c) order (integer)  
(d) number of passes (see XAPIIR documentation for all these parameters)  

Each line beginning **WF** is followed by the names of two SAC files.  

You then need to run the **ccdtest** program using a little script such as:  
```
#!/bin/sh
ev1=DPRK6
ev2=DPRK5
sta=IL01
pha=P
dtxtem=120.0
dtxtar=120.0
dtmpwn=2.0
dtarwn=3.5
icc=2
ccdtest $ev1 $ev2 $sta $pha $dtxtem $dtxtar $dtmpwn $dtarwn $icc < ccdtest.input
```

This should generate the output as follows with the example given:  
```
DPRK6  DPRK5  2017-09-03T03:39:05.6499 2016-09-09T00:39:05.2087  IL01   P   0.6726    -31028400.4412
```
(only the final line of the output is shown.)  

This program requires SAC and uses the XAPIIR library (Harris, 1990).
This whole library is provided in the single source file XAPIIR.f.  
(The XAPIIR routines are entirely third party software.)  







References:  

Harris, David. XAPiir: A recursive digital filtering package, report, September 21, 1990; California.  
(https://digital.library.unt.edu/ark:/67531/metadc1203741/m1/1/: accessed November 5, 2023),  
University of North Texas Libraries, UNT Digital Library,  
https://digital.library.unt.edu; crediting UNT Libraries Government Documents Department.  



