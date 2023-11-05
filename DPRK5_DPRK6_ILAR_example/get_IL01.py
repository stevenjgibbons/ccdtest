import obspy
from obspy.clients.fdsn import Client
client = Client("IRIS")
from obspy import UTCDateTime
t = UTCDateTime("2017-09-03T03:39:05.619049Z")
evcode       = "DPRK6"
stname       = "IL01"
network      = "IM"
location     = "--"
channel      = "SHZ"
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
