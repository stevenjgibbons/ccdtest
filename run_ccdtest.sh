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
