#!/usr/bin/env bash
#              bash 4.1.5(1)     Linux Ubuntu 10.04           Date : 2011-10-04
#
# _______________|  noise : ambient Brown noise generator (cf. white noise).
#
#           Usage:  noise  [minutes=59]  [band-pass freq center=1786]  [wave]
#                          ^minutes can be any positive integer.
#                           Command "noise 1" will display peak-level meter.
#
#    Dependencies:  play (from sox package)

#  Brownian noise, also known as Brown noise or red noise, is the kind of signal
#  noise produced by Brownian motion, hence its alternative name of random walk
#  noise. The term "Brown noise" does not originate from the color, but from 
#  Robert Brown who discovered Brownian motion.  Brown noise is produced by
#  integrating white noise.  The sound is a low roar resembling a waterfall or
#  heavy rainfall. We shall filter it through a band-pass, then add effects 
#  to mellow the output for ambience.

#  Volume oscillation (amplitude modulation) is used to simulate artificially 
#  long ocean waves. Try some of the free online services listed at the end 
#  of this file to hear other types of oscillation.
#
#        Our goal is to block out distractions for calm concentration.


#  CHANGE LOG  get LATEST version from https://bitbucket.org/rsvp/gists/src
#
#  2011-09-14  Add volume amplification to compensate for effects previously 
#                 applied. Use peak-level meter to avoid clipping.
#  2011-09-13  Add bass and treble tone controls.
#                 at loud volumes, low frequency thumping may be annoying, 
#                 so reduce gain on bass. Treble can be used to reduce 
#                 harshness from the high frequencies.
#                 Adjust default wave for tremolo to perfectly cycle 
#                 within the one-minute sample.
#  2011-09-12  Add end notes on the Cognitive Science aspects.
#  2011-09-11  Repeat use of one-minute segment to cut CPU usage by 95%.
#                 Fix tremolo to give very slow wave oscillation in volume.
#                 (Thanks to xguse for his gist at github.)
#                 Constant volume introduces tension psychologically.
#                 Code posted at https://gist.github.com/1209835
#  2011-09-10  First version based on 2009 article by Tom Swiss, and
#                 subsequent comments. See below for relevant portions.

#           _____ Prelims
# set -u
#   ^ unbound (i.e. unassigned) variables shall be errors.
#           Example of default assignment:    arg1=${1:-'foo'}
set -e
#   ^ error checking :: Highly Recommended (caveat:  you can't check $? later).  
#
# _______________     ::  BEGIN  Script ::::::::::::::::::::::::::::::::::::::::

minutes='60'
repeats=$(( minutes - 1 ))
center='1786'

wave='0.0333333'
#         ^increase for more volume oscillation, but suggest no higher than 0.20
#          (and no lower than 0.0166667). Its value should consider the 60
#          seconds duration of the repeated sample.

noise='brown'
#     ^your choice: 'white', 'pink', 'brown', 'tpdf'
#     where tpdf stands for Triangular Probability Density Function (cf. dither).
#     N.B. - white and pink noise have higher frequencies than Brown.

volume='1'

len='01:00'
#   ^CONSTANT one minute. (Format for specifying time length is hh:mm:ss.frac) 
#     ___ATTN___ We first pre-compute one minute of audio output to file, 
#     then playback repeatedly as necessary to satisfy minutes argument. 
#     This dramatically cuts CPU usage by 95% after the first minute.


#  For DEBUGGING: "noise 1" shows the peak-level meter; also instant production.
if [ $minutes -eq 1 ] ; then
     progress='--show-progress'
else
     progress='--no-show-progress'
fi


HELPTEXT="
	minutes		length of time to play the noise
	
	center		Apply a band-pass filter.  The frequency response drops
			logarithmically around the center frequency.  The width
			parameter gives the slope of the drop.  The frequencies at
			center + width and center - width will be half of their
			original amplitudes.  band defaults to a mode oriented to
			pitched audio, i.e. voice, singing, or instrumental music.
			The -n (for noise) option uses the alternate mode for un-
			pitched audio (e.g. percussion).  Warning: -n introduces a
			power-gain of about 11dB in the filter, so beware of output
			clipping.  band introduces noise in the shape of the filter,
			i.e. peaking at the center frequency and settling around it
	
	wave		increase for more volume oscillation, but suggest no higher than 0.20
			(and no lower than 0.0166667). Its value should consider the 60
			seconds duration of the repeated sample.
	
	noise		your choice: 'white', 'pink', 'brown', 'tpdf'
			where tpdf stands for Triangular Probability Density Function (cf. dither).
			N.B. - white and pink noise have higher frequencies than Brown.

	volume		less than 0 decreases the volume, more than 0 increases it. Be careful: 
			increasing this may cause clipping.
"

while getopts 'm:c:w:n:v:' OPTION
	do
		case $OPTION in
			m)	minutes="$OPTARG"
				;;
			c)	center="$OPTARG"
				;;
			w)	wave="$OPTARG"
				;;
			n)	noise="$OPTARG"
				;;
			v)	volume="$OPTARG"
				;;
			?)	printf "Usage: %s: [-m minutes <$minutes>] [-c center <$center>] [-w wave <$wave>] [-n noise <$noise>] [-v volume <$volume>]\n${HELPTEXT}" $(basename $0)
				exit 2
				;;
		esac
	done

echo " ::  Please stand-by... sox will 'play' $noise noise for $minutes minute(s)."
#          FYI  Channels: 2 @ 32-bit,  Samplerate: 48000Hz.


play $progress  -c 2  --null  synth  $len  ${noise}noise  \
     band -n $center 499               \
     tremolo $wave    43   reverb 19   \
     bass -11              treble -1   \
     vol     14dB                      \
     vol     $volume                   \
     repeat  $repeats

     
#  #    Previously published one-line versions misused tremolo:
#  play -c 2 --null synth $len brownnoise band -n 1800 1400 tremolo 500 .1 reverb 50
#  play -c 2 --null synth $len brownnoise band -n 2500 4000 tremolo 20 .1 reverb 50
#  play --null synth $len brownnoise band -n 1200 200 tremolo 20 .1 reverb 20
#  play --null synth $len brownnoise band -n 1200 200 tremolo 20 .1


#            _____ ARGUMENTS explained via "man sox"

#       -q, --no-show-progress
#           Run in quiet mode when SoX wouldn't otherwise do so; this is
#           the opposite of the -S option.
#  
#       -S, --show-progress
#           Display input file format/header information, and processing
#           progress as input file(s) percentage complete, elapsed time,
#           and remaining time (if known; shown in brackets), and the
#           number of samples written to the output file.  Also shown is a
#           peak-level meter, and an indication if clipping has occurred.


#       -c 2
#           Two channels. Without this, the output is not stereo.


#       -n, --null
#           This can be used in place of an input or output filename to
#           specify that a `null file' is to be used.  Note that here,
#           `null file' refers to a SoX-specific mechanism and is not
#           related to any operating-system mechanism with a similar name.
#           Using a null file to input audio is equivalent to using a
#           normal audio file that contains an infinite amount of silence,
#           and as such is not generally useful unless used with an effect
#           that specifies a finite time length (such as trim or synth).


#       synth
#           Some noise options: whitenoise, tpdfnoise, pinknoise, brownnoise.


#       band [-n] center[k] [width[h|k|o|q]]
#           Apply a band-pass filter.  The frequency response drops
#           logarithmically around the center frequency.  The width
#           parameter gives the slope of the drop.  The frequencies at
#           center + width and center - width will be half of their
#           original amplitudes.  band defaults to a mode oriented to
#           pitched audio, i.e. voice, singing, or instrumental music.
#           The -n (for noise) option uses the alternate mode for un-
#           pitched audio (e.g. percussion).  Warning: -n introduces a
#           power-gain of about 11dB in the filter, so beware of output
#           clipping.  band introduces noise in the shape of the filter,
#           i.e. peaking at the center frequency and settling around it
#
#  Consider this for centering the band-pass...
#  
#         Freq (Hz)    Octave      Description
#         16 to   32   1st         Human threshold, the lowest pedal 
#                                     notes of a pipe organ.
#         32 to  512   2nd to 5th  Rhythm frequencies, where the lower 
#                                     and upper bass notes lie.
#        512 to 2048   6th to 7th  Defines human speech intelligibility, 
#                                     horn-like or tinny sound quality.
#       2048 to 8192   8th to 9th  Gives presence to speech, where labial 
#                                     and fricative sounds lie.
#       8192 to 16384  10th        Brilliance, the sounds of bells and the
#                                     ringing of cymbals. In speech, sound
#                                     of letter "S" (8000-11000 Hz)
#            http://en.wikipedia.org/wiki/Audio_frequency
#
#  Avoid the really low frequencies which will produce disturbing rumble.


#       tremolo speed [depth]
#           Apply a tremolo (low frequency amplitude modulation) effect to
#           the audio.  The tremolo frequency in Hz is given by speed, and
#           the depth as a percentage by depth (default 40). Increasing
#           the depth gives wider range between soft and loud volumes.


#       reverb [-w|--wet-only] [reverberance (50%) [HF-damping (50%)
#           [room-scale (100%) [stereo-depth (100%)
#           [pre-delay (0ms) [wet-gain (0dB)]]]]]]


#       bass|treble gain 
#           Boost or cut the bass (lower) or treble (upper) frequencies of
#           the audio using a two-pole shelving filter with a response
#           similar to that of a standard hi-fi's tone-controls.  This is
#           also known as shelving equalisation (EQ).
#           gain gives the gain at 0 Hz (for bass), or whichever is the
#           lower of ∼22 kHz and the Nyquist frequency (for treble).  Its
#           useful range is about -20 (for a large cut) to +20 (for a
#           large boost).  Beware of clipping when using a positive gain.
#
#           When played loud, you may hear thumping bass lines in the 
#           case of brownnoise with effects. Reduce annoyance accordingly.


#       vol gain 
#           Apply an amplification or an attenuation to the audio signal.
#           Unlike the -v option (which is used for balancing multiple
#           input files as they enter the SoX effects processing chain),
#           vol is an effect like any other so can be applied anywhere,
#           and several times if necessary, during the processing chain.
#  
#           The amount to change the volume is given by gain which is
#           interpreted, according to the given type, as follows: 
#           if dB, then a power change in dB.  When type is dB, a gain 
#           of 0 leaves the volume unchanged, less than 0 decreases it, 
#           and greater than 0 increases it.
#           Beware of clipping when the increasing the volume.


#       repeat count
#           Repeat the entire audio count times.  Requires temporary file
#           space to store the audio to be repeated. [But where exactly?] 
             


#  _______________ "white noise" generator with sox [edited for code content]
#                   by Tom Swiss, http://unreasonable.org/node/303
#                   January 2007, updated circa September 2009,
#                   included comments through September 2011
#  
#  Sox is "the Swiss army knife of sound processing programs." It includes sound
#  generation capabilties for pure tones and white noise.  "Pink noise" is
#  also in sox's bag of tricks. After a bit of experimentation, I found the 
#  following shell script produced agreeable results:
#  
#  len='7:00:00'
#  play -t sl - synth $len pinknoise band -n 1200 200 tremolo 20 .1 < /dev/zero
#  
#       __________ Comments
#        
#  Drew Haven: This beats the heck out of "cat /dev/urandom > /dev/dsp". The band
#  filter is nice to take out the pops.
#  
#  gi1242: With recent versions of sox, things are a little simpler:
#       play -n synth 60:00 brownnoise
#  produces brown noise for an hour. (Replace brown with pink/white if you
#  prefer. My baby sleeps best with brown).
#  
#  Tom Swiss: "Brown" in "brown noise" means Brownian motion. It's also called
#  red noise. I learned something today, hooray!
#  
#  Adrien Beau, 30 January 2011: You can replace the "-t sl -" and "< /dev/zero"
#  parts with the "-n" option, so your sox invocation becomes:
#                  ^= --null (for null file)
#       play -n synth $len pinknoise band -n 1200 200 tremolo 20 .1
#  The brown noise sounds the best in my opinion.
#  
#  Dennis Murczak, 5 May 2011: I adapted the line to a "my neighbor is having a
#  party and I need to study" situation:
#       play -c 2 -n synth pinknoise band -n 2500 4000 reverb 20
#  The band pass is centered on human voice frequencies and wide enough to also
#  cover most of the musical frequency range, without producing annoying
#  high-pitched noise. The slight reverb adds a background/ambient quality for
#  less distraction. 


exit 0
# _______________ EOS ::  END of Script ::::::::::::::::::::::::::::::::::::::::


#           _____ Free ONLINE alternatives
#
#  Simply Noise for white, pink and brown/red noise generator; uses Flash:
#       http://simplynoise.com   (App is $0.99) 
#
#       [Flash consumes about 30 times more than our script in CPU usage!]
#
#  PlayNoise for white, pink, and brown noise generator; uses Javascript/HTML5:
#       http://playnoise.com
#
#  Random.org has 33-second sample audio files containing perfect white noise. 
#       The randomness comes from atmospheric noise, which is more natural 
#       than the pseudo-random number algorithms. Such files could serve as 
#       input to sox for further signal processing.
#       http://www.random.org/audio-noise/


#  _______________ HOW WHITE NOISE WORKS by Saabira Chaudhuri
#                  WSJ 31 Aug 2011
#  http://online.wsj.com/article/SB10001424053111904199404576538274265089288.html
#  
#  What people think of as "white noise" may actually be pink noise or brown
#  noise or any number of other colors.  Sound is associated with a color based
#  on where it falls on an audio spectrum of high to low frequencies. White noise
#  contains random sounds across all frequencies and "sounds very much like a
#  hiss because everything is changing in every sample," says Daniel Ellis,
#  associate professor of electrical engineering at Columbia University in New
#  York.  Pink noise, on the other hand, blends some high and lower frequencies,
#  so it sounds like a hiss with a low rumble, he adds. Brown noise shifts to the
#  lower end of the spectrum and sounds like rumbling.
#  
#  The most effective noise at blocking out other sounds is white noise because
#  it covers the largest range on the spectrum, says Andrew Catellier, an
#  electronics engineer at the Boulder, Colo.-based National Telecommunications &
#  Information Administration, which publishes a glossary of sounds' color
#  classifications.  Distinguishing noises by their frequencies is a useful tool
#  for scientists and engineers working on practical applications, such as
#  building a cellphone system or an ultrasound machine.  Sound is classified by
#  its audible frequencies and associated with a color based on where it falls on
#  the spectrum of high to low frequencies. White noise is unique in that it's
#  random and includes all frequencies -- akin to how white light has all the
#  colors in the spectrum.
#  
#  Calling sounds like rain or thunder white noise is somewhat of a misnomer, but
#  the makers of downloadable apps and sleep machines use the term anyway.  White
#  noise and other soothing sounds, once mainly played on machines to aid
#  nighttime sleep, are increasingly helping make daytime hours more serene.
#  [White noise is a common synthetic noise source used for sound masking by 
#  a tinnitus masker.]
#  
#  After HeavyDutyApps released an app called Sleep Pillow Ambiance to help
#  people sleep, it quickly realized that many customers used it during the day
#  as well. "The usage varies from people who need help concentrating while
#  working in noisy environments, commuters who need a break from train noise and
#  travelers that need a peaceful environment," says Benny Shaviv, chief
#  executive of the Westchester, N.Y.-based company. The $1.99 app has had more
#  than 1.6 million downloads, says Mr. Shaviv. "By January we were among the Top
#  50 apps in the Healthcare and Fitness category in iTunes."
#  
#  Most popular are sounds from nature: rain, wind, waves crashing on the beach
#  and crickets, Mr. Shaviv says. But the app also includes some unexpected
#  sounds, such as cold drink with ice, brushing hair and horse running in field.
#  Thunderstorm is the most popular downloaded noise.
#  
#  Developers of these apps say they frequently get requests for new sounds.
#  Steven Jian, co-owner of Simply Noise, has received requests for the sound of
#  passing cars and airport noises. Shaviv of HeavyDutyApps got a request for a
#  sonar noise from a former sailor who served on a submarine.  Todd Moore,
#  founder and CEO of TMSoft, the maker of an app called White Noise, says he
#  created a hair-dryer sound at one woman's request. "She told me that she could
#  not sleep without listening to it and that she had burned [out] six hair
#  dryers over the years."
#  
#  Daytime white-noise listeners say the sounds serve two main purposes: to block
#  out distractions and lessen sounds that cause anxiety, such as sirens.
#  "Certain types of noises can be relaxing," says Robert C. Fifer, director of
#  audiology and speech language pathology at the University of Miami. White
#  noise can be used to create a more relaxing working environment, masking
#  sounds and promoting a sense of privacy, he says.
#  
#  One small study examined white noise in a classroom environment. The research,
#  led by Goran Soderlund and Sverker Sikström of Stockholm University, looked at
#  51 students at a secondary school in Norway and found that those who normally
#  had difficulty paying attention performed better when white noise was added to
#  the classroom. The findings were published last year in the journal Behavioral
#  and Brain Functions.
#  
#  The authors theorized that white noise boosted neural activity, helping the
#  brain work more efficiently. The study predicted that white noise could help
#  children with attention deficit hyperactivity disorder (ADHD) learn to focus
#  on schoolwork better.

#           _____ REFERENCES
#
#  "The effects of background white noise on memory performance 
#       in inattentive school children"
#  Göran BW Söderlund1, Sverker Sikström, Jan M Loftesnes and EJ Sonuga-Barke
#  Behavioral and Brain Functions 2010, 6:55 doi:10.1186/1744-9081-6-55
#  Published: 29 September 2010 
#  http://www.behavioralandbrainfunctions.com/content/6/1/55/abstract


#  Re: Brown noise, see http://en.wikipedia.org/wiki/Brownian_noise

