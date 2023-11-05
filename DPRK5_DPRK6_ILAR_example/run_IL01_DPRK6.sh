#!/bin/sh
evcode='DPRK6'
origt=2017-09-03T03:30:01.6188
evlat=41.29520
evlon=129.07780
evdep=0.0
stlat=64.771599
stlon=-146.886093
stname='IL01'
network='IM'
location='--'
channel='SHZ'
python ../stateventphases/stateventabstimes.py  \
              --evlat $evlat --evlon $evlon --evdep $evdep \
              --stlat $stlat --stlon $stlon --origt $origt > tmp.txt
Ptime=`head -1 tmp.txt | awk '{print $3}'`
cat << EOF > get_IL01.py
import obspy
from obspy.clients.fdsn import Client
client = Client("IRIS")
from obspy import UTCDateTime
t = UTCDateTime("$Ptime")
evcode       = "${evcode}"
stname       = "${stname}"
network      = "${network}"
location     = "${location}"
channel      = "${channel}"
outfile      = evcode    + "_"
outfile     += stname    + "_"
outfile     += network   + "_"
outfile     += location  + "_"
outfile     += channel   + "_"
outfile     += ".sac"
[sbef, saft] = [120,120]
st = client.get_waveforms( network, stname, location, channel, t-sbef, t+saft )
st.resample( 100.0 )
# st.slice( t - 60.0, t + 60.0 )
st.write( outfile, format='SAC' )
st.plot()
EOF
