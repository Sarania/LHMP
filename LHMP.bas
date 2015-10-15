'LyricHero
#Define lhmpver "1.7 Phoenix"
#Define RGBA_R( c ) ( CUInt( c ) Shr 16 And 255 )
#define RGBA_G( c ) ( CUInt( c ) Shr  8 And 255 )
#define RGBA_B( c ) ( CUInt( c )        And 255 )
#define RGBA_A( c ) ( CUInt( c ) Shr 24         )
#Define ini_file 1
#Define playlist 4
#Include Once "inc/multiput.bi"
#Include Once "fmod.bi"
#Include Once "zlib.bi"
'#include once "inc/avireader.bas"
#Include Once "file.bi"
#Include Once "Inc/freetypeclass.bi"
#Include Once "inc/misc.bi"
#Include Once "inc/hqscale.bas"
#Include Once "fbgfx.bi"
Using fb
#Include Once "freeimage.bi"
#Include Once "inc/freetofb.bi"
#Include Once "inc/mp3tag.bi"
#Include Once "inc/drivelist.bi"
'Declare function ProcessFrame(AVIR As _AVI_READER Ptr, hDIB As HANDLE) As Any Ptr
Declare Function compute_scale(sourceimg As Any Ptr, destx As Integer = -1, desty As Integer = -1) As single
Declare Function chdrive() As Integer
Declare Function exitcheck() As Integer
Declare function load(ByVal doint As Integer = 1) As Integer
Declare Sub debug_info
Declare Sub rendersubs
Declare Sub fast_cls
Declare Sub cd_skip
Declare Sub load_cd
Declare Sub fmod_reset
Declare Sub loadsubtitle
Declare Sub options
Declare Sub stop_channel
Declare Sub clearvars
Declare Sub credits
Declare Sub basicgui
Declare Sub visualize
Declare Sub playmusic(track As String)
Declare Sub cleanup_and_exit
Declare Sub main_gui
Declare Sub clear_keys
Declare Sub reset_vol
Declare Sub fnf(ByVal errmsg As String)
Declare Sub bolt
Declare Sub changedir
Declare Sub nextsong
Declare Sub prevsong
Declare Sub loadsong
Declare Sub help_overlay
Declare Sub minitoggle
Declare Sub fillpix
Declare Sub cprint(fy As Integer, txt As String, fontuse as integer = 1)
Declare Sub DrawSpectrum(spectrum As Single Ptr)
Declare Sub starburst(ByVal cx As Double, ByVal cy As Double, ByVal r As Double,ByVal e As Double, ByVal k As Integer)
Declare Sub tstar(ByVal cx As Double, ByVal cy As Double, ByVal r As Double, ByVal e As Double,ByVal k As Integer)

Type song
	filename As String
	title As String = "----"
	artist As String = "----"
	album As String = "----"
	hasArt As UInteger = 0
	artsize As UInteger
End Type

'holds subtitles
Type subtitle
	layer As UInteger
	start As String
	End As String
	startsec As Single
	endsec As Single
	style As String
	speaker As String
	L_offset As UInteger
	R_offset As UInteger
	V_offset As UInteger
	effect As String
	text(1 To 10) As String
End Type

Randomize

'Const TRUE = 1
'Const FALSE = 0
Const As Double s = .05             ' unity vector ( c + j s )
Const As Double c = Sqr(1 - s * s)
Const PI As Double = 3.1415926535897932

'color constants
Const white = RGB(255,255,255)
Const red = RGB(255,0,0)
Const maroon = RGB(128,0,0)
Const green = RGB(0,128,0)
Const lime = RGB(0,255,0)
Const olive = RGB(128,128,0)
Const blue = RGB(0,0,255)
Const teal = RGB(0,128,128)
Const navy = RGB(0,0,128)
Const aqua = RGB(0,255,255)
Const fuschia = RGB(255,0,255)
Const purple = RGB(128,0,128)
Const yellow = RGB(255,255,0)
Const black = RGB(0,0,0)
Const grey = RGB(128,128,128)
Const silver = RGB(192,192,192)
Const orange = RGB(255,165,0)


Dim Shared CurSong As song
Dim Shared As event e
Dim Shared As Integer curpos = 1, viz_mode = 6, frames, random_on = 0, fps = 30
Dim Shared As Integer screenx, screeny,fullx, fully, fonts,  menusel = 1, mousex, mousey, maxsel, helpon=0, minimode = 0
Dim Shared As Integer songlength, songlengthmin, songlengthsec, pressed, winx, winy, movin, mpvol = 255, volbarfade, scrnotice
dim Shared As integer mousewheel, repeat = 0, m3u = FALSE, loaded_bg = 0, loaded_image = 0
Dim Shared As Double  updtime, videopos, audiopos, osd_mcolor
Dim Shared As Integer debug = 1, frames_rendered, frames_skipped, vid_x_offset=0, vid_y_offset=0, renderer=2
Dim Shared As String  smode, lastdir, m3ufile, osd_message
Dim Shared As Integer channel = 1, subs_mode = FALSE, subs_avail = FALSE, avircheck = FALSE, osd_mtimer
Dim Shared As Single starsize(1 To 3), treblesize, minicount, zoom_mult=1, suboffset = 0, audio_delay = 0
Dim Shared As Integer Ptr trackhandle
Dim Shared As Integer cd_track = 1
Dim Shared As Any Ptr img, background_image
Dim Shared As String curdrive
Dim Shared vizstring (0 To 8) As String
'Dim Shared AVIR As _AVI_READER Ptr
Dim Shared As String mode, list(1 To 100)
mode = "Audio"
Dim Shared As subtitle subs(1 To 5000)
vizstring(0) = "Off"
vizstring(1) = "Starburst"
vizstring(2) = "Blue pattern"
vizstring(3) = "Circle waveform over blue pattern"
vizstring(4) = "Rainbow cycle circle waveform over blue pattern"
vizstring(5) = "Waterpulse"
vizstring(6) = "Colorpulse"
vizstring(7) = "Background image"
vizstring(8) = "Album art"

'get the screenres and whether to fullscreen or not
If Not FileExists("lhmp.ini") Then cleanup_and_exit
Open "lhmp.ini" For Input As #ini_file
Input #ini_file, screenx
Input #ini_file, screeny
Input #ini_file, smode
Input #ini_file, lastdir
Close #ini_file

If screenx < 640 Or screenx > 1980 Or screeny < 320 Or screeny > 1080 Then
	'reset it because it got messed up somehow
	screenx = 800
	screeny = 600
EndIf

If UCase(smode) <> "WINDOWED" And UCase(smode) <> "FULLSCREEN" And UCase(smode) <> "WINFRAME" Then
	'Again this is only triggered if the inifile is mucked about in manually
	Smode = "Windowed"
EndIf
fullx = screenx
fully = screeny

'buffers for the data used in visualization routines
Dim Shared As Single vizbuf(1 To screenx/8), trebuf(1 to screenx/8), midbuf(1 to screenx/8)


'compute font based on screeny, sketchy but works reasonably well
fonts = CInt(screeny/24)
'but not smaller than 20
If fonts < 20 Then fonts = 20

'Load fonts
Dim Shared As truetype font
If font.init Then Stop
If font.get_font("fonts/scribble.ttf")=0 Then Stop
font.set_render_mode(FT_RENDER_MODE_NORMAL)
font.set_screen_size(screenx,screeny)
font.set_size(fonts)
font.set_color(RGB(255,255,255))
font.set_back_color(RGB(0,0,0))

'Subtitle/Lyric font
Dim Shared As truetype subfont
If subfont.init Then Stop
If subfont.get_font("fonts/FreeSans.ttf")=0 Then Stop
subfont.set_render_mode(FT_RENDER_MODE_NORMAL)
subfont.set_screen_size(screenx,screeny)
subfont.set_size(fonts)
subfont.set_color(RGB(255,255,255))
subfont.set_back_color(RGB(0,0,0))


WindowTitle("LHMP " & lhmpver)

'Set screenmode based on loaded parameters
If UCase(smode) = "FULLSCREEN" Then
	ScreenRes screenx,screeny,32, 2, GFX_FULLSCREEN
ElseIf UCase(smode) = "WINFRAME" Then
	ScreenRes screenx,screeny,32, 2
Else
	ScreenRes screenx,screeny,32,2, GFX_NO_FRAME
EndIf

ScreenSet 1,0


'FMOD initialization
FSOUND_Init(44100, 8, 0)
FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE)
load 'Read directory or playlist into list() array
If mode <> "CD" Then loadsong 'because the CD loading is called as soon as CD is chosen

'Main loop
updtime = Timer 'Used in FPS calc
Do
	audiopos = fsound_stream_getTime(trackhandle)/1000
	audiopos + = audio_delay 'Add the user selectable offset
	If viz_mode <> 11 Then 'Viz_mode 11 is VIDEO, so this triggers on AUDIO
		videopos=audiopos
		If frames / (Timer-updtime) < fps Then

			If Timer - updtime > 2 Then 'rolling fps timer
				frames = frames / (Timer-updtime)
				updtime = Timer - 1
			EndIf

			main_gui
		Else
			Sleep 15
		End If
	Else 'VIDEO syncronization code
		'videopos = avir->currentframe/avir->fps
		If videopos > audiopos Then
			Sleep (videopos*1000) - (audiopos*1000)
		ElseIf audiopos - videopos > .15 Then 'video is behind audio
			While audiopos - videopos > .15 'so skip frames til it catches up
				frames_skipped+=1
		'		avir->currentframe+=1
		'		videopos = avir->currentframe/avir->fps
			Wend
		EndIf
		main_gui
	End If

	'check for keypresses, this is the most "realtime" way I have come up with
	'inkey, multikey either lag or have repeating/missed key problems
	while (ScreenEvent(@e))
		scrnotice = 1000
		SetMouse(,,1)
		Select Case e.type
			Case EVENT_KEY_PRESS
				Select Case e.scancode
					Case sc_o 'options
						FSOUND_SetPaused(channel,TRUE)
						options
						While MultiKey(SC_SPACE):Sleep 1,1:Wend
						fsound_SetPaused(channel, FALSE)
					
					Case sc_w 'Renderer change 1 = Multiput, 2 = RotateScaleHQ
						if renderer = 1 then renderer = 2 else renderer = 1
						osd_mtimer = 500
						osd_message = "Renderer: "
						If renderer = 1 Then
							osd_message + = "Nearest Neighbor (Multiput)"
						ElseIf renderer = 2 Then
							osd_message + = "Bilinear (RotateScaleHQ)"
						End If
						osd_mcolor = yellow
						
					Case sc_s 'Subtitle mode change, 1 = Normal, 2 = In shaded box. 0 = Off
						if subs_avail = TRUE Then 'You can't change this mode if there are no subs/lyrics
							subs_mode + = 1
							If subs_mode = 3 Then subs_mode = 0
							osd_mtimer = 500
							osd_message = "Subtitles: "
							If subs_mode = 1 Then
								osd_message & = "Enabled"
							ElseIf subs_mode = 2 Then
								osd_message & = "In shaded box"
							Else
								osd_message &= "Disabled"
							EndIf
							osd_mcolor = aqua
						EndIf

					Case sc_end 'Delay subtitles more
						suboffset+=.25
						osd_mtimer = 500
						osd_message = "Subtitle Delay: " & suboffset & " seconds."
						osd_mcolor = navy

					Case sc_home 'Delay subtitles less
						suboffset-=.25
						osd_mtimer = 500
						osd_message = "Subtitle Delay: " & suboffset & " seconds."
						osd_mcolor = navy

					Case sc_plus And mode = "Video" 'delay audio more
						audio_delay+=.05
						osd_mtimer = 500
						osd_message = "Audio Delay: " & audio_delay & " seconds."
						osd_mcolor = navy

					Case sc_minus And mode = "Video" 'delay audio less
						audio_delay-=.05
						osd_mtimer = 500
						osd_message = "Audio Delay: " & audio_delay & " seconds."
						osd_mcolor = navy

					Case sc_insert And mode = "Video"'force resync
					'	FSOUND_Stream_SetTime(trackHandle, ((avir->currentframe/avir->fps)*1000))
						osd_mtimer = 500
						osd_message = "Video/Audio resync forced!"
						osd_mcolor = purple

					Case sc_pageup And mode = "Video"'zoom in
						zoom_mult+=.1
						osd_mtimer = 500
						osd_message = "Zoomy: " & zoom_mult
						osd_mcolor = maroon

					Case sc_pagedown And mode = "Video"'zoom out
						zoom_mult-=.1
						If zoom_mult < .3 Then zoom_mult = .3
						osd_mtimer = 500
						osd_message = "Zoomy: " & zoom_mult
						osd_mcolor = maroon

					Case sc_i And mode = "Video" 'pan video up
						vid_y_offset-=10*zoom_mult
						osd_mtimer = 500
						osd_message = "Video Y offset: " & vid_y_offset
						osd_mcolor = orange

					Case sc_k And mode = "Video" 'pan video down
						vid_y_offset+=10*zoom_mult
						osd_mtimer = 500
						osd_message = "Video Y offset: " & vid_y_offset
						osd_mcolor = orange

					Case sc_j And mode = "Video" 'pan video left
						vid_x_offset-=10*zoom_mult
						osd_mtimer = 500
						osd_message = "Video X offset: " & vid_x_offset
						osd_mcolor = orange

					Case sc_l And mode = "Video" 'pan video right
						vid_x_offset+=10*zoom_mult
						osd_mtimer = 500
						osd_message = "Video X offset: " & vid_x_offset
						osd_mcolor = orange

					Case sc_tilde 'show the debug infos
						If debug = FALSE Then debug = TRUE Else debug = FALSE
						
					Case sc_f 'Toggle fullscreen
						If UCase(smode) = "FULLSCREEN" Then
							smode = "WINDOWED"
							ScreenRes screenx,screeny,32,2, GFX_NO_FRAME
						Else
							If UCase(smode) = "WINDOWED" Then
								smode = "FULLSCREEN"
								ScreenRes screenx,screeny,32,2, GFX_FULLSCREEN
							End if
						EndIf
						ScreenSet 1,0
						
					Case sc_m ' mini mode toggle
						minitoggle
						
					Case sc_x ' random toggle
						If random_on = 0 And repeat = 1 Then repeat = 0
						If random_on = 1 Then random_on = 0 Else random_on = 1
						repeat = 0
					Case sc_r 'repeat toggle
						If repeat = 0 And random_on = 1 Then random_on = 0
						If repeat = 1 Then repeat = 0 Else repeat = 1

					Case sc_space 'Pause toggle
						FSOUND_SetPaused(channel,TRUE)
						cprint screeny/2, "Paused"
						ScreenCopy
						While MultiKey(SC_SPACE):Sleep 1,1: Wend
						While Not MultiKey(SC_SPACE)
							'note the program will be in this block exclusively while paused
							If exitcheck Then
								cleanup_and_exit
							EndIf

							Sleep 15
						Wend
						while (ScreenEvent(@e)): Sleep 1,1: Wend
						fsound_SetPaused(channel, FALSE)


					Case sc_h 'Help overlay
						If helpon = 1 Then helpon = 0 Else helpon = 1

					Case sc_d 'Change directory
						If minimode = 1 Then minitoggle
						changedir
						If mode = "CD" Then Exit Select 'CD loading is called directly from chdrive which is called from changedir
						clearvars
						load(0)
						loadsong

					Case sc_v 'Visualizer toggle
						If mode <> "Video" Then
							osd_mtimer = 500
							viz_mode+=1
							If viz_mode = 3 Then viz_mode = 5 'Temp skip for broken modes
							If viz_mode = 8 And loaded_bg = 1 Then
								loaded_bg = 0
								ImageDestroy(background_image)
							EndIf
							If viz_mode = 9 Then viz_mode = 0
							If viz_mode = 7 Then
								If FileExists(ExePath & "/bg.jpg") And loaded_bg = 0 Then
									background_image = freeimage_load_fb(ExePath & "/bg.jpg", TRUE) 'user supplied image
									loaded_bg = 1
								ElseIf FileExists(ExePath & "/graphics/animegirl.jpg") And loaded_bg = 0 then
									background_image = freeimage_load_fb(ExePath & "/graphics/animegirl.jpg", TRUE) 'fallback image
									loaded_bg = 1
								Else
									If loaded_bg = 0 Then viz_mode = 8 'if all else fails... then nothing.
								EndIf
							EndIf
							If viz_mode = 0 Or viz_mode = 7 Or viz_mode = 8 Then
								fps = 5 'Static image so low fps
							Else
								fps = 120 'moving image so more fps
							EndIf
							frames = 0 'fps reset
							updtime = Timer
							osd_message = vizstring(viz_mode)
							osd_mcolor = green
						End If

					Case SC_LEFTBRACKET ' Previous track
						If mode="CD" Then
							cd_track-=1
							If cd_Track < 1 Then cd_track = 1
							cd_skip
						Else
							prevsong
							loadsong
						End If
						
					Case SC_RIGHTBRACKET ' Next track
						If mode="CD" Then
							cd_track+=1
							If cd_track > fsound_stream_getnumsubstreams(trackhandle) Then cd_track = 1
							cd_skip
						Else
							nextsong
							loadsong
						End If
						
					Case sc_left 'Seek backwards
						If viz_mode <> 11 Then 'Audio seek
							FSOUND_Stream_SetTime(trackHandle, fsound_stream_getTime(trackhandle)-5000)
						Else 'video and audio seek
						'	frames_rendered-= CInt(avir->fps*20)
						'	avir->currentframe - = CInt(avir->fps*20)
						'	FSOUND_Stream_SetTime(trackHandle, ((avir->currentframe/avir->fps)*1000)-(audio_delay*1000))
						End If

					Case sc_right 'Seek forwards
						If viz_mode <> 11 Then 'Audio seek
							FSOUND_Stream_SetTime(trackHandle, fsound_stream_getTime(trackhandle)+5000)
						Else 'video and audio seek
						'	frames_rendered+= CInt(avir->fps*20)
'							avir->currentframe + = CInt(avir->fps*20)
'							FSOUND_Stream_SetTime(trackHandle, ((avir->currentframe/avir->fps)*1000)-(audio_delay*1000))
						End If

					Case sc_up ' Volume up
						mpvol+=10
						If mpvol > 255 Then mpvol = 255
						volbarfade = 255
						reset_vol

					Case sc_down ' Volume down
						mpvol-=10
						If mpvol < 0 Then mpvol = 0
						volbarfade=255
						reset_vol

				End Select
				
			Case EVENT_MOUSE_WHEEL 'Wheel is bound to volume
				If mousewheel > e.z Then ' Volume Down
					mpvol-=10
					If mpvol < 0 Then mpvol = 0
					volbarfade=255
					mousewheel = e.z
					reset_vol
				Else ' Volume up
					mpvol+=10
					If mpvol > 255 Then mpvol = 255
					volbarfade=255
					mousewheel = e.z
					reset_vol
				EndIf

			Case EVENT_MOUSE_MOVE
				If pressed And movin Then ' moving the window
					ScreenControl GET_WINDOW_POS, winx, winy
					ScreenControl SET_WINDOW_POS, winx + (e.dx), winy + (e.dy)
				EndIf
				mousex=e.x
				mousey=e.y

			Case event_mouse_button_release
				pressed = 0
				movin = 0

			case EVENT_MOUSE_BUTTON_PRESS
				pressed = -1
				If mousey < 50 And minimode = 0 Then
					movin = 1 ' you can only grab the top of the window
				ElseIf mousey > screeny-43 And mousey < screeny - 23 Then
					If mousex > (screenx/2)+ 110 And mousex < (screenx/2)+130 Then ' next track button
						nextsong
						loadsong
					ElseIf mousex > (screenx/2)-130 And mousex < (screenx/2)-110 Then ' previous track button
						prevsong
						loadsong
					ElseIf mousex > (screenx/2)-100 And mousex < (screenx/2)+100 Then 'Seek based on pbar click
						If viz_mode <> 11 Then 'Audio seek
							FSOUND_Stream_SetTime(trackHandle, ((mousex- ((screenx/2)-100)   )/200)*fsound_stream_getlengthms(trackhandle))
						Else 'video and audio seek
						'	avir->currentframe = ((mousex- ((screenx/2)-100)   )/200) * avir->numframes
						'	FSOUND_Stream_SetTime(trackHandle, ((avir->currentframe/avir->fps)*1000)-(audio_delay*1000))
						End If
					End If
				End If

		End Select
	Wend
	If exitcheck Then cleanup_and_exit
Loop


Sub playmusic(track As String)
	If Not FileExists(track) Then fnf(track)
	If trackhandle Then trackhandle = NULL
	trackHandle = FSOUND_Stream_Open(track, FSOUND_MPEGACCURATE, 0, 0 ) 'MPEGACCURATE for Variable bit rates
	FSOUND_Stream_Play(1, trackHandle)
	reset_vol
	songlength = FSOUND_Stream_GetLengthMs(trackhandle)
	songlengthmin=0
	songlengthsec=0

	'convert the ms into min:sec
	While songLength > 59999
		songLengthMin+=1
		songLength=songlength-60000
	Wend

	songLengthSec = songLength/1000
	If songlengthsec = 60 Then
		songlengthmin+=1
		songlengthsec=0
	EndIf
	songlength = FSOUND_Stream_GetLengthMs(trackhandle) 'for comparing ms based timer stuff too, reget the ms
End Sub

Sub cleanup_and_exit
	If loaded_bg = 1 Then ImageDestroy(background_image)
	if loaded_image = 1 Then ImageDestroy(img)
'	If avir Then Delete avir
	Close
	fsound_close
	Open ExePath & "/lhmp.ini" For Output As #ini_file
	Print #ini_file, screenx
	Print #ini_file, screeny
	Print #ini_file, smode
	Print #ini_file, CurDir
	Close #ini_file
	End
End Sub

Sub main_gui
	Dim As Integer curmin, cursec
	Dim As String cursecstr, songlengthsecstr, timestr
	fast_cls
	If fsound_stream_getTime(trackhandle) >= songlength Then 'End of song
		If mode = "CD" Then
			cd_Track+=1
			If cd_track > fsound_stream_getnumsubstreams(trackhandle) Then cd_track = 1
			cd_skip
		Else
			nextsong
			loadsong
		End If
	EndIf
	frames+=1
	If minimode = 1 Then
		Dim As String mininotice 'Small space in minimode so we use a counter to choose what info is shown
		If Timer - minicount < 2 Then
			mininotice = cursong.title
		ElseIf Timer - minicount >= 2 And Timer - minicount < 4 Then
			mininotice = cursong.artist
		ElseIf Timer - minicount >=4 And Timer - minicount < 6 Then
			mininotice = cursong.album
		ElseIf Timer-minicount >=6 then
			minicount = Timer
			mininotice = cursong.title
		EndIf

		font.set_size 10
		cprint (screeny-8), mininotice
		font.set_size fonts
		dim as integer volperc = (mpvol/255) * 100
		For volx As Integer = 1 To 10 'Volume indicators for mini mode
			Line (5+volx, screeny-5)-(5+volx, Screeny-5 - (.40*volperc)), green
			Line (screenx-5-volx,screeny-5)-(screenx-5-volx, screeny-5 - (.40*volperc)), green
		Next
	EndIf
	If minimode = 0 Then
		'These are the various things that fade out, volume bar, visualizer name, etc
		'Well, these are the vars that control the display/hide/fade
		'the actual display is a ways down
		If volbarfade > 0 Then
			if fps = 30 Then volbarfade-=8 Else volbarfade -= 48 'we want it on the screen a consistent time, so if FPS is lower, subtract more from the countdown
			If volbarfade < 0 Then volbarfade = 0
		EndIf
		If osd_mtimer > 0 Then
			If fps = 30 Then osd_mtimer - = 8 Else osd_mtimer -= 48 ' same as for volbarfade
			If osd_mtimer < 0 Then osd_mtimer = 0
		EndIf
		If scrnotice > 0 Then
			If fps = 30 Then scrnotice - = 8 Else scrnotice -=48 ' same as above
			If scrnotice < 0 Then
				if mode = "Fullscreen" then SetMouse(,,0)
				scrnotice = 0
			EndIf
		EndIf

		'get track pos
		curPos = fsound_stream_gettime(trackhandle)
		curpos/=1000 ' convert to seconds
		While curpos > 59'convert to min:sec
			curmin+=1
			curpos-=60
		Wend
		curSec = curpos
		If viz_mode <> 0 Then
			visualize
		EndIf
		If (mode = "Audio" Or smode <> "FULLSCREEN" Or mode = "CD") Then basicgui
		' here are the things that only display *SOMETIMES*
		' like the volume bar, visualization name, zoom amount, etc
		If volbarfade > 0 Then 'Volume bar in main mode
			pBar(255,mpvol,screenx/2-88,screeny/2+10,175,RGB(0,0,volbarfade),volbarfade)
			font.set_color RGB(0,volbarfade,0)
			cprint(screeny/2+28, Str(CInt((mpvol/255)*100)) & "%")
			font.set_color white
		EndIf
		If osd_mtimer > 0 Then 'Various OSD messages are passed through here
			font.set_size 30
			font.set_color osd_mcolor
			cprint screeny/2-100, osd_message
			font.set_size fonts
			font.set_color white
		EndIf

		If subs_mode <> 0 Then rendersubs
		If scrnotice = 0 And debug = 0 And mode = "Video" Then
			'We can stop here because the other stuff need not be rendered
			'in video mode. We want as much clean space as possible.
			ScreenCopy
			return
		EndIf

		font.set_color white

		'all the text that goes on screen, computed to scale
		If debug = TRUE  Then
			debug_info
		Else
			If scrnotice > 0 Then 'Only visible for a few seconds after a SCREENEVENT or track change
				font.print_text screenx - (font.get_text_width(time))-10, screeny-30, Time
				font.print_text screenx - (font.get_text_width(date))-10, screeny-10, Date
				font.set_size(fonts/2)
				font.print_text 10, screeny-10, "LHMP version " & lhmpver
				font.set_size fonts
			End if
			font.set_size(fonts/2)
			font.print_text 10, 80, "Title: " & cursong.title
			font.print_text 10, 110, "Artist: " & cursong.artist
			font.print_text 10, 140, "Album: " & cursong.album

			if repeat then
				font.set_color fuschia
				cprint screeny-48, "Repeat"
				font.set_color white
			ElseIf random_on Then
				font.set_color yellow
				cprint screeny-48, "Random"
				font.set_color white
			EndIf

			font.set_size(fonts)
			If cursec < 10 Then cursecstr = "0" & Str(cursec) Else cursecstr = Str(cursec) 'Pad the cursec and lengthsec with a 0 if necessary
			If songlengthsec <10 Then songlengthsecstr = "0" & Str(songlengthsec) Else songlengthsecstr = Str(songlengthsec)
			timestr =  Str(curmin) & ":" & cursecstr & "/" & Str(songlengthmin) & ":" & songlengthsecstr
			font.set_color(white)
			font.set_size(fonts)
		End if

		If cursong.hasart = 1 And viz_mode < 7 Then 'Don't display the art in mode 7 or 8
			'scale the album art based on screensize and art aspect ratio
			Dim As Integer iwidth, iheight
			Dim As Single scalex, scaley, perc
			ImageInfo(img,iwidth,iheight)
			scalex = screenx/iwidth
			scaley = screeny/iheight
			If scaley < scalex Then
				perc = scaley
			Else
				perc = scalex
			EndIf
			perc / = 4
			multiput(,2+((iwidth*perc)/2), screeny-((iheight*perc)/2)-2, img,perc,perc)
		EndIf
		If helpon=1 Then help_overlay
	End If
	pBar(songlength, fsound_stream_getTime(trackhandle),(screenx/2)-100,screeny-43,200,blue)
	Box ((screenx/2)+110,screeny-43,20,20,white)
	Box ((screenx/2)-130,screeny-43,20,20,white)
	If mousey > screeny-43 And mousey < screeny - 23 Then
		If mousex > (screenx/2)+ 110 And mousex < (screenx/2)+130 Then
			For i As Integer = 1 To 19
				Line ((screenx/2)+111,screeny-43+i)-((screenx/2)+129,screeny-43+i),purple
			Next
		ElseIf mousex > (screenx/2)-130 And mousex < (screenx/2)-110 Then
			For i As Integer = 1 To 19
				Line ((screenx/2)-111,screeny-43+i)-((screenx/2)-129,screeny-43+i),purple
			Next
		End if
	End if
	'right arrow
	Line ((screenx/2)+110,screeny-33)-((screenx/2)+130,screeny-33),white
	Line -((screenx/2)+115,screeny-40),white
	line((screenx/2)+130,screeny-33)-((screenx/2)+115,screeny-26),white
	'left arrow
	Line ((screenx/2)-110,screeny-33)-((screenx/2)-130,screeny-33),white
	Line -((screenx/2)-115,screeny-40),white
	Line((screenx/2)-130,screeny-33)-((screenx/2)-115,screeny-26),white
	If minimode = 0 Then 
		cprint screeny-25, timestr
	Else
		font.set_size 13
		If repeat = 1 Then
			font.set_color fuschia
			cprint screeny-28, "Repeat"
			font.set_color white
		ElseIf random_on = 1 Then
			font.set_color yellow
			cprint screeny-28, "Random"
			font.set_color white
		EndIf
		font.set_size fonts
	EndIf
	ScreenCopy
End Sub

Function load(ByVal doint As Integer = 1) As Integer
	Dim As Integer k, a
	Dim As String f
	Dim As Integer j = 0
	If doint = 0 Then GoTo justload
	'read list of songs you can play THIS IS A PLACEHOLDER COMMENT RIGHT NOW
	k = 0
	ChDir(lastdir) 'Change to last known dir
	curdrive = Left(lastdir,3) 'Extract current drive as well
	changedir
	If mode = "CD" Then
		Return 0 'Again cd loading happens in chdrive
	EndIf
	justload:
	If m3u = TRUE Then 'Read list of files into list() from the M3u
		Open m3ufile For Input As playlist
		Dim As String tempstr
		Do While Not Eof(playlist)
			Line Input #playlist, tempstr
			If UCase(Left(tempstr,4)) = "FILE" Then tempstr = Right(tempstr, Len(tempstr)-6)
			If Left(tempstr, 1) <> "#" And tempstr <> "" And FileExists(tempstr) And (UCase(Right(tempstr,4)) = ".MP3" Or UCase(Right(tempstr,4)) = ".OGG" Or UCase(Right(tempstr,4)) = ".WAV") Then
				k+=1
				list(k) = tempstr
			EndIf
		Loop
		Close playlist
	End If
	If m3u = FALSE Then 'Read list of files into list() from directory
		F = Dir("*",32,@A)
		While F <> ""
			If UCase(Right(F,4)) = ".MP3" Or UCase(Right(F,4)) = ".WAV" Or UCase(Right(F,4)) = ".OGG" Or UCase(Right(F,4)) = ".AVI" Then
				k+=1
				list(k) = f
			EndIf
			F = Dir(A)
		Wend
	End if
	maxsel = k 'max amount of songs
	fast_cls
	menusel -=1 'because we are about to call nextsong which will +1 to this, which seems silly... I may need to look into it
	nextsong
	font.set_size(fonts)
	Return 0
End Function

Sub clear_keys
	'Clear all the various key buffers
	While MultiKey(SC_UP) Or MultiKey(SC_DOWN) Or MultiKey(SC_LEFT) Or MultiKey(SC_RIGHT) Or MultiKey(SC_SPACE) Or MultiKey(SC_ENTER) Or InKey <> ""
		Sleep 1,1
	Wend
	While ScreenEvent(@e) Or InKey$ <> ""
		Sleep 1,1
	Wend
End Sub

Sub reset_vol
	fsound_setvolumeabsolute(channel, mpvol)
End Sub

Sub fnf(ByVal Errmsg As String)
	'FILE NOT FOUND error
	fast_cls
	font.set_size(20)
	font.set_color red
	font.print_text 10,20, "File not found: " & errmsg
	font.print_text 10,50, "Exit in 5 seconds..."
	ScreenCopy
	Sleep 5000, 1
	cleanup_and_exit
End Sub


Sub DrawSpectrum (spectrum As Single Ptr)
	'borrowed from psymp3 player, plots the DSP spectrum analyzer
	Dim X As Integer
	For X = 0 To 320
		Line(screenx-260+x,screeny-(spectrum[x]*(160)))-(screenx-260+x,screeny), _
		IIf(x>200,RGB((x-200)*2.15,0,255),IIf(x<100, _
		RGB(128,255,x*2.55),RGB(128-((x-100)*1.28),255 _
		- ((x-100)*2.55),255))), bf
	Next X
End Sub

'this belongs to richard@freebasic but I modified it to make a starburst
Sub starburst(ByVal cx As Double, ByVal cy As Double,	ByVal r As Double, ByVal e As Double, ByVal k As Integer)    
	Dim As Double t, x = r, y = 0
	PSet(x + cx, cy), 0     ' start circle
	Dim As Integer i = 0
	For seg As Integer = 1 To 126' segments
		i + = 1
		t = x
		x = t * c - y * s
		y = t * s + y * c
		If i = 1 Then
			Line (cx,cy)-(x + cx, y * e + cy), k
			If i = 1 Then
				tstar(x + cx, y * e + cy,treblesize*180,1,RGB(255,0,160))
			EndIf
		EndIf
		If i = 3 Then i = 0
	Next seg
End Sub

Sub bolt 'Draw a pseudorandom lightning bolt to the screen
	Dim As Integer boltx = Rnd*screenx, bolty=0, boltdir = 2
	'1 = SW
	'2 = S
	'3 = SE
	Do
		Select Case boltdir
			Case 1
				boltx-=1
			Case 2
				'
			Case 3
				boltx+=1
		End Select
		bolty+=1
		PSet  (boltx,bolty),RGB(255,255,254)
		If Rnd*100 <60 Then
			boltdir = Rnd*100
			If boltdir < 45 Then boltdir = 1
			If boltdir > 45 And boltdir < 55 Then boltdir = 2
			If boltdir > 55 Then boltdir = 3
		End If
	Loop Until bolty = screeny
End Sub

Sub changedir 'Hacked up chdir sub, needs reworked AGAIN I think
	mode = "Default"
	top:
	Dim As String F
	Dim As String listofdirs(1 To 100), listoffiles(1 To 100), olddir, listofm3u(1 To 100)
	Dim As UInteger dirind = 1, maxdirs, dirsel = 1, maxfiles, fileind=1, m3uind = 1, maxm3u, cout
	Dim As Integer a, slashfind
	F = Dir("*",23,@A) 'Get list of DIRECTORIES
	While F <> ""
		If (A And 16) = 16 Then
			listofdirs(dirind) = F
			dirind+=1
		EndIf
		F = Dir(A)
	Wend

	filerefresh:
	olddir = CurDir
	ChDir listofdirs(dirsel)
	For fileind=1 To 100 'clear file array
		listoffiles(fileind)=""
	Next
	fileind=1
	F = Dir("*",32,@A)
	While F <> "" 'Get list of files (But not playlist type files)
		If UCase(Right(F,4)) = ".MP3" Or UCase(Right(F,4)) = ".WAV" Or UCase(Right(F,4)) = ".OGG" Or UCase(Right(F,4)) = ".AVI" Then
			listoffiles(fileind) = f
			fileind+=1
		EndIf
		F = Dir(A)
		If fileind = 11 Then
			listoffiles(11) = "Further files omitted from list..."
			Exit While
		EndIf
	Wend
	ChDir olddir
	m3uind = 1
	F = Dir("*",32,@A)
	While F <> "" 'Get list of playlist type files
		If UCase(Right(F,4)) = ".M3U" Or UCase(Right(F,4)) = ".PLS" Then
			listofm3u(m3uind) = f
			m3uind+=1
		EndIf
		F = Dir(A)
	Wend
	maxm3u = m3uind-1
	maxdirs = dirind - 1
	maxfiles = fileind
	fast_cls
	basicgui
	Draw String (10, 60), "Directory Index of:" 'Dirsel UI
	If Len(CurDir) > 50 Then
		For i As Integer = 50 To 100 Step 1
			If Left(Right(CurDir,i),1) = "\" Or  Left(Right(CurDir,i),1) = "/" Then
				Draw String (10, 70), "..." & Right(CurDir,i)
				For p As Integer = 1 To i
					Draw String (5+(p*8),75),"_"
				Next
				Exit for
			End If
		Next
	Else
		Draw String (10, 70), Right(CurDir,50)
		For p As Integer = 1 To Len(CurDir)
			Draw String (5+(p*8),75),"_"
		Next
	End If

	For dirind = 1 To maxdirs Step 1
		If Dirind = dirsel Then
			If fileind > 1 Then
				Draw String(10, 80 + dirind*10), Left(listofdirs(dirind),50) & " <--",green
			Else
				Draw String(10, 80 + dirind*10), Left(listofdirs(dirind),50) & " <--", red
			End If

		Else
			Draw String(10, 80 + dirind*10), left(listofdirs(dirind),50)
		End If
	Next
	For m3uind = 1 To maxm3u Step 1
		if dirsel = m3uind + maxdirs Then
			Draw String(10, 80 + (dirind*10) + ((m3uind-1)*10)), listofm3u(m3uind) & " <--", blue
		Else
			Draw String(10, 80 + (dirind*10) + ((m3uind-1)*10)), listofm3u(m3uind), silver
		End If
	Next
	Draw String (screenx-150, screeny-100), "Up/Down - Navigate"
	Draw String (screenx-250, screeny-80), "Space - Browse to new directory"
	Draw String (screenx-180, screeny-60), "Enter - Confirm choice"
	Draw String (screenx-130, screeny-40), "C - Change drive"
	Draw String (screenx-110, screeny-20), "Escape - Quit" 'end of dirsel UI
	Do
		If ScreenEvent(@e) Then
			clear_keys
			Select Case e.scancode
				Case sc_up
					dirsel-=1
					If dirsel=1 Then dirsel = maxdirs + maxm3u
					GoTo filerefresh

				Case sc_down
					Dirsel+=1
					If dirsel > maxdirs + maxm3u Then dirsel = 2
					GoTo filerefresh

				Case sc_space
					ChDir listofdirs(dirsel)
					GoTo top

				Case sc_enter
					If dirsel > maxdirs Then
						m3u = TRUE 'playlist mode
						m3ufile = listofm3u(dirsel-maxdirs)
						return
					EndIf
					If maxfiles > 1 Then
						ChDir listofdirs(dirsel)'regular mode
						lastdir = listofdirs(dirsel)
						m3u = FALSE
						Return
					End If

				Case sc_c
					cout = chdrive
					clear_keys
					If cout = 1 Then 'back to top with new drive
						GoTo top
					ElseIf cout = 2 Then 'CD mode
						load_cd
						Return
					End If
			End Select
		End if
		ScreenCopy
		Sleep 75
		If exitcheck Then cleanup_and_exit
	Loop
End Sub

Sub nextsong
	stop_channel
	Close
	If random_on = 0 Then
		If repeat = 0 Then menusel+=1
	Else
		menusel = CInt(Rnd*maxsel)+1
	End if
	If menusel > maxsel Then menusel = 1
	cursong.filename = list(menusel)
End Sub
Sub prevsong
	stop_channel
	Close
	If menusel = 1 Then menusel = maxsel Else menusel-=1
	cursong.filename = list(menusel)
End Sub

Sub loadsong
	Dim As Any Ptr tagname
	scrnotice = 1000
	cursong.title = cursong.filename
	If avircheck = TRUE Then
		avircheck = FALSE
		'Delete avir
	EndIf
	If UCase(Right(cursong.filename,4)) = ".AVI" Then
'		AVIR = new _AVI_READER(cursong.filename)
		avircheck = TRUE
		mode = "Video"
		channel = 71
		viz_mode = 11
		loadsubtitle
	Else
		if viz_mode = 11 then viz_mode = 5
		mode = "Audio"
		channel = 1
		loadsubtitle
	EndIf
	fmod_reset

	If UCase(Right(list(menusel),4)) = ".MP3" Then
		trackHandle = FSOUND_Stream_Open(list(menusel),0, 0, 0 )
		If getmp3tag("ID3", list(menusel)) = "1" Then
			cursong.title = getmp3tag("title", list(menusel))
			cursong.artist = getmp3tag("artist", list(menusel))
			cursong.album= getmp3tag("album", list(menusel))
		end If
		cursong.hasart = FSOUND_Stream_FindTagField(trackhandle, FSOUND_TAGFIELD_ID3V2, "APIC", @tagname, @cursong.artsize)
		fsound_Stream_close(trackhandle)
	End If

	'get the album art if it is there
	If loaded_image Then
		ImageDestroy(img)
		loaded_image = false
	EndIf
	If cursong.hasart = 1 Then
		img = getalbumart(cursong.filename, cursong.artsize)
		loaded_image = 1
	ElseIf FileExists(ExePath & "/graphics/noart.png") then
		img = freeimage_load_fb(ExePath & "/graphics/noart.png", TRUE)
		loaded_image = 1
		cursong.hasart = -1
	Else
		loaded_image = 0
		cursong.hasart = 0
	EndIf
	clear_keys
	playmusic(cursong.filename)
End Sub


Sub tstar(ByVal cx As Double,	ByVal cy As Double, ByVal r As Double, ByVal e As Double, ByVal k As Integer)
	'Treble stars
	Dim As Double t, x = r, y = 0
	PSet(x + cx, cy), 0     ' start circle
	Dim As Integer i = 0
	For seg As Integer = 1 To 126' segments
		i + = 1
		t = x
		x = t * c - y * s
		y = t * s + y * c
		If i = 4 Then
			Line (cx,cy)-(x + cx, y * e + cy), k
			i = 0
		EndIf
	Next seg
End Sub

'function ProcessFrame(AVIR As _AVI_READER Ptr, hDIB As HANDLE) As Any Ptr 'Process AVI Frame by vdecampo with small mod for writing to fb image by me
'	Dim BIH  As BITMAPINFOHEADER
'	Dim As HANDLE pDIB
'	Dim As Any Ptr pPIX
'	Dim As fb.image Ptr imageout = ImageCreate (avir->sizex, avir->sizey)
'	Dim As Any Ptr imgdata
'
'	ImageInfo(imageout,,,,,imgdata)
'	pDIB=GlobalLock(hDIB) 'Get pointer to DIB
'
'	If pDIB Then
'		memcpy @BIH,pDIB,SizeOf(BITMAPINFOHEADER)'Get BITMAPINFOHEADER
'		pPIX = pDIB+BIH.biSize
'		AVIR->FlipBuffer(pPIX)
'		AVIR->RGB_RGBA (pPIX, imgdata, BIH.biSizeImage\3)'RGB->RGBA
'	EndIf
'
'	GlobalUnlock hDIB     'Release DIB pointer
'	Return imageout
'End Function

Sub visualize
	If mode = "CD" Then
		If viz_mode  <> 7 Then Return 'Viz doesn't work with CD. Dunno why.
	EndIf
	If fsound_isplaying(channel) = 0 Then Return 'Because reading when no music is playing is probably bad
	Dim As Single Ptr specbuf
	Dim As Single flashbright
	Dim As uinteger smallres = screeny
	If screenx < screeny Then smallres = screenx

	If mode = "Audio" Then
		starsize(1) = 0
		specbuf = FSOUND_DSP_GetSpectrum()

		For i As Integer = 0 To 36 Step 1
			If i < 19 Then starsize(1)+ = specbuf[i+4] Else treblesize+=specbuf[i]
			If i < 6 Then flashbright + = specbuf[i]
		Next
		flashbright / = 6
		treblesize/=18
	End If

	Select Case viz_mode

		Case  Is = 1 'Purple star thing
			For i As Integer = 1 To screeny Step 1
				Line (0,i)-(screenx,i),RGB(0,0,128*flashbright)
			Next
			DrawSpectrum(specbuf)
			If flashbright > .3 Then  bolt
			If starsize(1)*100 > (smallres/2)-30 Then starsize(1) = ((screeny/2)-30)/100
			starsize(1) = (starsize(1)+starsize(2))/2
			starsize(2) = starsize(1)
			starburst(screenx/2,screeny/2,starsize(1)*100,1,purple)

		Case Is = 2, Is = 3, Is = 4 'Partially broken
			Dim as Integer	viz2x = 512
			dim as Integer	viz2y = 512
			Dim As Any Ptr backbuf = ImageCreate(viz2x,viz2y,black)
			For i As Integer = screenx/8 To 2 Step - 1
				vizbuf(i) = vizbuf(i-1)
			Next

			vizbuf(1) = flashbright

			For i As Integer = 1 To viz2x/8 Step 1 'draw half of blue pattern into buffer
				For p As Integer = 0 To 7
					Line backbuf,(viz2x/2,viz2y/2)-((i*8)-p,viz2y), RGB(0,0,255*vizbuf(i))
					Line backbuf,(viz2x/2,viz2y/2)-(viz2x+8-(i*8)-p,0), RGB(0,0,255*vizbuf(i))
				Next
			Next

			For i As Integer = 1 To viz2y/8 Step 1 'draw other half of blue pattern into buffer
				For p As Integer = 0 To 7
					Line backbuf, (viz2x/2,viz2y/2)-(viz2x,(i*8)-p), RGB(0,0,255*vizbuf(i))
					Line backbuf, (viz2x/2,viz2y/2)-(0,viz2y+8-(i*8)-p), RGB(0,0,255*vizbuf(i))
				Next
			Next

			multiput(,screenx/2,screeny/2,backbuf,screenx/viz2x,screeny/viz2y) 'scale buffer onto screen
			ImageDestroy(backbuf)

		Case Is = 5 'Water pulse
			For i As Integer = screenx/8 To 2 Step - 1
				vizbuf(i) = vizbuf(i-1)
			Next
			vizbuf(1) = flashbright
			For i As Integer = 1 To screenx/8 Step 1
				For ii As Integer = 0 To (screenx/8)/25
					Circle (screenx/2,screeny/2),(i*((screenx/8)/25))-ii, RGB(0,127*vizbuf(i),255*vizbuf(i))
				Next
			Next
			fillpix

		Case Is = 6 'Color pulse
			For i As Integer = screenx/8 To 2 Step - 1
				vizbuf(i) = vizbuf(i-1) 'bass and BLUE
				trebuf(i) = trebuf(i-1) 'treble and GREEN
				midbuf(i) = midbuf(i-1) 'mid and RED
			Next
			midbuf(1) = starsize(1)/8 
			vizbuf(1) = flashbright
			trebuf(1) = treblesize*2
			For i As Integer = 1 To screenx/8 Step 1 'draw increasingly larger circles based on RGB computed via music DSP
				dim as uinteger r,g,b
				b = 255*vizbuf(i)
				g = 255*trebuf(i)
				r = 255*midbuf(i)

				For ii As Integer = 0 To (screenx/8)/25
					Circle (screenx/2,screeny/2),(i*((screenx/8)/25))-ii, RGB(r,g,b)
				Next
			Next
			fillpix

		Case Is = 7 'BG image 
			Dim As Single scaleperc = compute_scale(background_image,screenx,screeny-50)
			If renderer = 1 Then
				multiput(,screenx/2, (screeny+50)/2, background_image,scaleperc, scaleperc)
			ElseIf renderer = 2 Then
				Dim As fb.image Ptr tempptr = ImageCreate(screenx,screeny,black)
				rotatescalehq(tempptr,background_image, screenx/2,screeny/2+25,0,scaleperc)
				Put(0,0),tempptr,PSet
				ImageDestroy(tempptr)
			End If

		Case Is = 8 'Album Art
			If cursong.hasart <> 0 Then
				Dim As Single scaleperc = compute_scale(img,screenx,screeny-50)
				If renderer = 1 Then
					multiput(,screenx/2, (screeny+50)/2, img,scaleperc,scaleperc)
				ElseIf renderer = 2 Then
					Dim As fb.image Ptr tempptr = ImageCreate(screenx,screeny,black)
					rotatescalehq(tempptr,img, screenx/2,screeny/2+25,0,scaleperc)
					Put(0,0),tempptr,PSet
					ImageDestroy(tempptr)
				End If
			EndIf

		Case Is = 11 'Video
'			Dim As handle hDIB = AVIR->GetNextFrame()
'			If hDIB Then
'				frames_rendered+=1
'				Dim As Any Ptr frameptr = ProcessFrame(AVIR, hDIB) 'frameptr points to the AVI frame we are about to scale
'				Dim As Single perc = compute_scale(frameptr,screenx,screeny)
'				perc*=zoom_mult 'Apply user controlled zoom
'				If renderer = 1 Then 'NN scaling
'					multiput(,screenx/2+vid_x_offset,screeny/2+vid_y_offset,frameptr,perc,perc)
'				ElseIf renderer = 2 Then 'Bilinear scaling
'					Dim As fb.image Ptr tempptr = ImageCreate(screenx,screeny,black)
'					rotatescalehq(tempptr,frameptr,screenx/2+vid_x_offset,screeny/2+vid_y_offset,0,perc)
'					Put(0,0),tempptr,pset
'					ImageDestroy(tempptr)
'				End if
'				ImageDestroy(frameptr)
'			End If
	End Select
End Sub


Sub basicgui
	' just the border and title, put here because many things need it
	Line(0,50)-(screenx-1,50), white
	Line(0,0)-(screenx-1,0),white
	Line(screenx-1,0)-(screenx-1,screeny-1),white
	Line(screenx-1,screeny-1)-(0,screeny-1), white
	Line(0,screeny-1)-(0,0),white

	For i As Integer = 1 To 49
		Line (1,i)-(screenx-2,i),navy
	Next
	cprint(32, "LHMP") 'Title
End Sub

Sub credits 'all those people whose work is someone used in this program. Unless I forgot someone. Sorry!
	Dim As Any Ptr fbcredit, fmodcredit
	clear_keys
	fbcredit = freeimage_load_fb("graphics/fblogo.png", TRUE)
	fmodcredit = freeimage_load_fb("graphics/fmodcredit.png", TRUE)
	font.set_size(CInt(screeny/40))
	MultiPut(,screenx/2,screeny/2,fbcredit,(screenx/400)/2,(screeny/182)/2)
	font.print_text 50,50, "Compiled with FreeBASIC version " + Str(__FB_VER_MAJOR__) + "." + Str(__FB_VER_MINOR__) + "." + Str(__FB_VER_PATCH__)
	ScreenCopy
	Sleep 2000
	If exitcheck Then cleanup_and_exit
	fast_cls
	MultiPut(,screenx/2,screeny/2,fmodcredit,(screenx/640)/2,(screeny/325)/2)
	font.print_text 50,50,"Produced using sound effect libraries from FMOD."
	font.print_text 50, 90, "FMOD Sound System copyright Firelight technologies Pty Ltd 1994-2010"
	font.print_text 50, 130, "FMOD is free for non-commercial use."
	ScreenCopy
	Sleep 2000
	If exitcheck Then cleanup_and_exit
	fast_cls
	cprint(screeny/2-20,"This software uses the FreeImage open source image library.")
	cprint(screeny/2, "See http://freeimage.sourceforge.net for details.")
	cprint(screeny/2+20, "FreeImage is used under the FIPL, version 1.0")
	ScreenCopy
	Sleep 3000
	If exitcheck Then cleanup_and_exit
	fast_cls
	ImageDestroy(fbcredit)
	ImageDestroy(fmodcredit)
	font.print_text 10,screeny/2-160, "Additional Credits:"
	font.print_text 10,screeny/2-120, "Multiput function belongs to D.J. Peters of the FreeBASIC forum"
	font.print_text 10,screeny/2-80, "RotateScaleHQ belongs to Mysoft of the FreeBASIC forum"
	font.print_text 10, screeny/2-40, "Freetype class for FreeBASIC belongs to Thorham of the FreeBASIC forum"
	font.print_text 10,screeny/2, "zlib Copyright (C) 1995-2005 Jean-loup Gailly and Mark Adler"
	font.print_text 10, screeny/2+40, "Portions of this software are copyright © 2007 The FreeType Project"
	font.print_text 10, screeny/2+80, "Levenshtein Distance code belongs to sir_mud of the FreeBASIC forum"
	font.print_text 10, screeny/2+120, "VFW implementation based on code provided by Vincent DeCampo"
	font.print_text 10, screeny/2+160, "FreeImage to FBImage code belongs to cha0s of the FreeBASIC forum"
	ScreenCopy
	Sleep 4000
	If exitcheck Then cleanup_and_exit
	fast_cls
	font.set_size 56
	cprint(screeny/2-30,"Written by:")
	font.set_color fuschia
	cprint(screeny/2+50,"~Blyss~")
	font.set_size fonts
	font.set_color white
	ScreenCopy
	Sleep 2000
End Sub

Sub clearvars
	'Sometimes these vars need cleared, mostly when things are unloaded/loaded
	Dim blank As song
	Dim As subtitle blanksub
	close
	For i As Integer = 1 To 100
		list(i) = ""
	Next
	For i As Integer = 1 To 100
		subs(i) = blanksub
	Next
	subs_mode = 0
	subs_avail = FALSE
	suboffset = 0
	zoom_mult = 1
	vid_x_offset = 0
	vid_y_offset = 0
	cursong = blank
	menusel = 1
	maxsel=0
	songlength=0
	songlengthmin=0
	songlengthsec=0
	pressed=0
	movin=0
	curpos=1
	updtime = Timer
	frames = 0
End Sub

Sub stop_Channel 'Stop fmod playing
	FSOUND_Stream_Stop(trackHandle)
	FSOUND_Stream_Close(trackHandle)
End Sub

Sub fmod_reset 'Reset FMOD because of the hackish way we feed it AVI its the only way to stop AVI track w/o loading a new AVI
	fsound_close
	FSOUND_Init(44100, 8, 0)
	FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE)
End Sub

Sub help_overlay 'Help screen is not up to date!
	box(screenx/2-200,screeny/2-200,400,400,RGB(255,253,254))
	Paint(screenx/2-190,screeny/2-190),RGBA(100,100,100,100),rgb(255,253,254)
	Draw String (screenx/2-190,screeny/2-190), "Active in Media Player mode only:", navy
	Draw string (screenx/2-190,screeny/2-170), "<- Seek ->"
	Draw String (screenx/2-190,screeny/2-150), "Click on song progress bar - Seek"
	Draw String (screenx/2-190,screeny/2-130), "[ Change Track ]"
	draw string (screenx/2-190,screeny/2-110), "Space - Pause track"
	Draw String (screenx/2-190,screeny/2-90), "R - Toggle repeat"
	Draw String (screenx/2-190,screeny/2-70), "X - Toggle random"
	Draw String (screenx/2-190,screeny/2-50), "/|\ Volume \|/"
	draw string (screenx/2-190,Screeny/2-30), "V - Visualizer mode change"
	draw string (screenx/2-190,screeny/2-10), "D - Change directory"
	draw string (screenx/2-190,screeny/2+10), "M - Toggle mini player"
	Draw String (screenx/2-190,screeny/2+30),"Active in both modes:", navy
	draw string (screenx/2-190,screeny/2+50), "Tab - Scrolling lyric display on/off"
	draw string (screenx/2-190,screeny/2+70), "ESC - Exit the program"
	Draw String (screenx/2-190,screeny/2+190), "H - Toggle this overlay"
End Sub

Sub options 'Hacked up options menu
	If minimode = 1 Then minitoggle
	Dim As String wintype = UCase(smode),wtext
	Dim As Integer xres = screenx, yres=screeny, msel = 1, xupd=0, changed = 0
	Do
		fast_cls
		basicgui
		box(screenx/2-200,screeny/2-200,400,400,RGB(255,253,254))
		Paint(screenx/2-190,screeny/2-190),RGBA(100,100,100,100),rgb(255,253,254)
		Draw String (screenx/2-190,screeny/2-190), wtext, IIf(msel=1,navy,white)
		Draw String (screenx/2-190,screeny/2-170), Str(xres), IIf(msel=2 Or msel=3,navy,white)
		Draw String (screenx/2-190,screeny/2-150), str(yres), IIf(msel=3 Or msel=2,navy,white)
		Draw String (screenx/2-100,screeny/2+190), "Space - Exit options menu"
		If MultiKey(SC_UP) Then
			clear_keys
			msel - = 1
			If msel < 1 Then msel = 1
		EndIf
		If MultiKey(SC_DOWN) Then
			clear_keys
			msel +=1
			If msel > 2 Then msel = 2
		EndIf

		If MultiKey(SC_RIGHT) Then
			changed = 1
			clear_keys
			Select Case msel
				Case Is = 1
					If wintype = "FULLSCREEN" Then
						wintype = "WINDOWED"
					ElseIf wintype = "WINDOWED" Then
						wintype = "WINFRAME"
					Else wintype = "FULLSCREEN"
					EndIf
				Case Is = 2
					xupd=1
					Select Case xres
						Case Is = 640
							xres = 800
						Case Is = 800
							xres = 1024
						Case Is = 1024
							xres = 1280
						Case Is =1280
							xres = 1366
						Case Is = 1366
							xres = 1920
						Case Else
							xres = 640
					End Select
			End Select
		EndIf
		If MultiKey(SC_LEFT) Then
			changed = 1
			clear_keys
			Select Case msel
				Case Is = 1
					If wintype = "FULLSCREEN" Then
						wintype = "WINFRAME"
					ElseIf wintype = "WINFRAME" Then
						wintype = "WINDOWED"
					Else wintype = "FULLSCREEN"
					EndIf
				Case Is = 2
					xupd=1
					Select Case xres
						Case Is = 1920
							xres = 1366
						Case Is = 1366
							xres = 1280
						Case Is = 1280
							xres = 1024
						Case Is = 1024
							xres = 800
						Case Is = 800
							xres = 640
						Case Else
							xres = 1920
					End Select
			End Select
		EndIf

		If xupd = 1 Then 'Choose yres based on xres
			xupd = 0
			Select Case xres
				Case Is = 640
					yres = 480
				Case Is = 800
					yres = 600
				Case Is = 1024
					yres = 768
				Case Is = 1280
					yres = 720
				Case Is = 1366
					yres = 768
				Case Is = 1920
					yres = 1080
			End Select
		EndIf
		If wintype = "FULLSCREEN" Then 'Make some slightly more user friendly text than the notation used in code
			wtext = "Full Screen"
		ElseIf wintype = "WINFRAME" Then
			wtext = "OS Managed Frame"
		Else
			wtext = "Windowed"
		End if
		ScreenCopy
		Sleep 30
		If exitcheck Then cleanup_and_exit
	Loop While Not MultiKey(SC_SPACE) And Not MultiKey(SC_ENTER)
	Open ExePath & "/lhmp.ini" For Output As ini_file 'Save to ini file!
	Print #1, xres
	Print #1, yres
	Print #1, wintype
	Print #1, curdir
	Close ini_file
	screenx = xres
	screeny = yres
	fullx = screenx
	fully = screeny
	If changed = 1 Then 'reset anything that depends on screenx, screeny, or window type

		font.set_screen_size(screenx,screeny)
		subfont.set_screen_size(screenx,screeny)
		'compute font based on screeny, sketchy but works reasonably well
		fonts = CInt(screeny/24)
		'but not smaller than 20
		If fonts < 20 Then fonts = 20

		Select Case wintype
			Case "WINFRAME"
				smode = "WINFRAME"
				ScreenRes screenx, screeny,32,2
			Case "FULLSCREEN"
				smode = "FULLSCREEN"
				ScreenRes screenx, screeny,32,2, GFX_FULLSCREEN
			Case Else
				SMODE = "WINDOWED"
				ScreenRes screenx, screeny,32,2, GFX_NO_FRAME
		End Select
		ScreenSet 1,0
		changed = 0
	End if
End Sub

Sub cprint(fy As Integer, txt As String, fontuse As Integer = 1) 'Print text with font on centered X coordinate
	If fontuse = 1 Then 'Scribble
		font.print_text(screenx/2 - (font.get_text_width(txt)/2), fy, txt)
	ElseIf fontuse = 2 Then 'Arial (Well, open source equivalent anyway)
		subfont.print_text(screenx/2 - (subfont.get_text_width(txt)/2), fy, txt)
	EndIf
End Sub

Function compute_scale(sourceimg As Any Ptr, destx As Integer = -1, desty As Integer = -1) As Single
	'Aspect correct scaling of image computation, return value is percent to scale in BOTH dimensions
	If destx = -1 Then destx = screenx
	If desty = -1 Then desty = screeny
	Dim As UInteger iwidth, iheight
	Dim As Single scalex, scaley, perc
	ImageInfo(sourceimg,iwidth,iheight)
	scalex = destx/iwidth
	scaley = desty/iheight
	If scaley < scalex Then
		perc = scaley
	Else
		perc = scalex
	EndIf
	Return perc
End Function

Sub minitoggle 'toggle miniplayer
	If mode = "Video" Then	Return 'no mini mode for vids you noob!
	If minimode = 0 Then
		viz_mode = 0
		frames = 0
		minimode = 1
		minicount = timer
		screenx = 300
		screeny = 50
		ScreenRes screenx, screeny, 32, 2, GFX_ALWAYS_ON_TOP
		ScreenSet 1,0
		font.set_screen_size(screenx,screeny)
		subfont.set_screen_size(screenx,screeny)
	Else
		minimode = 0
		screenx = fullx
		screeny = fully
		screenres screenx,screeny, 32, 2
		screenset 1,0
		font.set_screen_size(screenx,screeny)
		subfont.set_screen_size(screenx,screeny)
	EndIf
End Sub

Sub loadsubtitle 'load subtitles/lyrics since we now store our lyrics in the SSA format
Dim As Integer subcount = 0
Dim As subtitle blanksub
For i As Integer = 1 To 5000
	subs(i) = blanksub
Next
Dim As String dummy, hours, mins, secs, hunds, extension
If FileExists(Left(cursong.filename,Len(cursong.filename)-4) & ".ass") Then
	extension = ".ass"
ElseIf FileExists(Left(cursong.filename,Len(cursong.filename)-4) & ".ssa") Then
	extension = ".ssa"
ElseIf FileExists(Left(cursong.filename,Len(cursong.filename)-4) & ".srt") Then
	extension = ".srt"
Else
	subs_mode = 0
	subs_avail = false
	Return
EndIf
Dim As Integer f= freefile
Open (Left(cursong.filename,Len(cursong.filename)-4) & extension) For Input As #f

If extension = ".ass" Or extension = ".ssa" Then
Do While Not Eof(f)
Input #f, dummy 'eat up the header and other stuff we aren't looking at

If UCase(Left(dummy,9)) = "DIALOGUE:" Then 'its a subtitle line
subcount+=1
subs(subcount).layer = cint(Right(dummy,1))
Input #f, subs(subcount).start
Input #f, subs(subcount).end
Input #f, subs(subcount).style
Input #f, subs(subcount).speaker
Input #f, subs(subcount).L_offset
Input #f, subs(subcount).R_offset
Input #f, subs(subcount).V_offset
Input #f, subs(subcount).effect
'Up to this point only delimiting commas are allowed
'But there can be commas in the actual text so we use LINE INPUT
Line Input #f, subs(subcount).text(1)


'A lot of times style information precedes the text in a format like
'{\be1} and this code eats that up and throws it away
top:
If Left( subs(subcount).text(1),1) = "{" Then
	Do
		subs(subcount).text(1) = Right(subs(subcount).text(1),Len(subs(subcount).text(1))-1)

		If Left(subs(subcount).text(1),1) = "}" Then
			subs(subcount).text(1) = Right(subs(subcount).text(1),Len(subs(subcount).text(1))-1)
			Exit Do
		End If

		If subs(subcount).text(1) = "" Or Len(subs(subcount).text(1)) = 0 Then
			Exit Do 'Something went wrong because we are out of characters, so break from loop
		EndIf

	Loop
EndIf
If Left( subs(subcount).text(1),1) = "{" Then GoTo top

'This finds the \N and splits the lines based on it
For i As Integer = 1 To 9
	If InStr(UCase(subs(subcount).text(i)), "\N") Then
		subs(subcount).text(i+1) = Right(subs(subcount).text(i), Len(subs(subcount).text(i))  - (InStr(UCase(subs(subcount).text(i)), "\N") + 1)    )
		subs(subcount).text(i) = Left(subs(subcount).text(i), InStr(UCase(subs(subcount).text(i)), "\N")-1)
	EndIf
next
'This will convert the h:mm:ss:hh time format into just seconds
'so we can compare it to TIMER values easily
'First the start point
hours = Left(subs(subcount).start,1)
mins = Right(Left(subs(subcount).start,4),2)
secs = Right(Left(subs(subcount).start,7),2)
hunds = Right(subs(subcount).start,2)
subs(subcount).startsec + = CSng( CInt(hunds)/100) + CInt(secs) + (CInt(mins)*60) + (CInt(hours)*3600)

'Then the end point
hours = Left(subs(subcount).end,1)
mins = Right(Left(subs(subcount).end,4),2)
secs = Right(Left(subs(subcount).end,7),2)
hunds = Right(subs(subcount).end,2)
subs(subcount).endsec + = CSng( CInt(hunds)/100) + CInt(secs) + (CInt(mins)*60) + (CInt(hours)*3600)
End If
Loop
End If

If extension = ".srt" Then 'SRT format must be handled differently
	Dim As String timesplit, eatvar = "hungry" 'lol
	Do While Not Eof(f)
		subcount+=1
		Input #f, dummy
		Line Input #f, timesplit

		For i As Integer = 1 To 10
			Line Input #f, subs(subcount).text(i)
			If subs(subcount).text(i) = "" Then Exit For 'Blank line delimiter in SRT
		Next

		If subs(subcount).text(10) <> "" Then 'If there is more than 10 lines per frame for some reason then the eatvar eats them, but then it gets hungry on the next frame
			While eatvar <> ""
				Line Input #f, eatvar
			Wend
			eatvar = "hungry"
		EndIf
		'Convert SRT time format(hh:mm:ss:hhh) to internal LHMP timeformat(seconds as single)
		'Start sec
		subs(subcount).start = Left(timesplit,12)
		subs(subcount).end = right(timesplit,12)
		hours = Left(subs(subcount).start,2)
		mins = Right(Left(subs(subcount).start,5),2)
		secs = Right(Left(subs(subcount).start,8),2)
		hunds = Right(subs(subcount).start, 3)
		subs(subcount).startsec + = CSng( CInt(hunds)/1000) + CInt(secs) + (CInt(mins)*60) + (CInt(hours)*3600)
		'End sec
		hours = Left(subs(subcount).end,2)
		mins = Right(Left(subs(subcount).end,5),2)
		secs = Right(Left(subs(subcount).end,8),2)
		hunds = Right(subs(subcount).end, 3)
		subs(subcount).endsec + = CSng( CInt(hunds)/1000) + CInt(secs) + (CInt(mins)*60) + (CInt(hours)*3600)

	Loop
EndIf
Close #f
subs_avail = true
subs_mode = 1
End Sub
Sub load_cd 'Load CDDA
	clearvars
	fmod_reset
	trackHandle = FSOUND_Stream_Open(Left(curdrive,2), FSOUND_Normal, 0, 0 )
	cd_track = 1
	cd_skip
End Sub

Sub cd_skip 'Has to be handled differently than other track skips because substream, although I may integrate it later
	fsound_stream_setsubstream(trackhandle, cd_Track)
	songlength = FSOUND_Stream_GetLengthMs(trackhandle)
	songlengthmin=0
	songlengthsec=0

	'convert the ms into min:sec
	While songLength > 59999
		songLengthMin+=1
		songLength=songlength-60000
	Wend

	songLengthSec = songLength/1000
	If songlengthsec = 60 Then
		songlengthmin+=1
		songlengthsec=0
	EndIf
	songlength = FSOUND_Stream_GetLengthMs(trackhandle)
	cursong.filename = list(cd_track)
	cursong.title = "CD Track " & cd_track
	FSOUND_Stream_Play(1, trackHandle)
End Sub


Function chdrive As Integer 'Change drive interface
	Dim As _drivelist drives
	Dim As uinteger drivesel = 1
	Do
		fast_cls
		basicgui
		Draw String (10,60), "Available drives: "
		Draw String (10,70), "__________________"
		For i As Integer = 1 To drives.count
			Draw String(10,70+(i*10)), drives.drive(i),IIf(drivesel = i, green, white)
		Next
		Draw String (screenx-150, screeny-80), "Up/Down - Navigate"
		Draw String (screenx-180, screeny-60), "Enter - Confirm choice"
		Draw String (screenx-270, screeny-40), "TAB - Play as compact disc (CDDA)"
		Draw String (screenx-110, screeny-20), "Escape - Quit"
		if ScreenEvent(@e) Then
			clear_keys
			Select Case e.scancode
				Case sc_down
					drivesel+=1
					If drivesel > drives.count -1 Then drivesel = drives.count -1

				Case sc_up
					drivesel - = 1
					If drivesel < 1 Then drivesel = 1

				Case sc_tab
					mode = "CD"
					curdrive = drives.drive(drivesel)
					ChDir(curdrive)
					Return 2 'Tell changedir to play CD

				Case sc_space
					curdrive = drives.drive(drivesel)
					ChDir(curdrive)
					Return 1 'Tell changedir to reload from new drive

			End Select
		End If
		ScreenCopy
		Sleep 75
		If exitcheck Then cleanup_and_exit
	Loop
End function

Function exitcheck As Integer 'Check for exit inducing stuff and return 1 if true
	If MultiKey(SC_ESCAPE) Or InKey = Chr(255) & "k" Then
		Return 1
	Else
		Return 0
	EndIf
End Function

Sub fast_cls 'Because CLS isn't fast enough
	For i As Integer = 1 To screeny
		Line(0,i)-(screenx,i), black
	Next
End Sub

sub rendersubs 'Render the subtitles to screen
	Dim As Integer fontsize = fonts, subtop = (screeny-40)-(300), widepoint=0
	for i as integer = 1 to 5000 'We check all 5000 each frame because SSA/ASS MAY NOT BE IN ORDER
		If (audiopos-audio_delay)-suboffset > subs(i).startsec And (audiopos-audio_delay)-suboffset< subs(i).endsec Then

			For ti As Integer = 1 To 10
				If subfont.get_text_width(subs(i).text(ti)) > screenx Then 'we must split the line
					'First make space by moving the other lines forward
					For it As Integer = 10 To ti+2 Step -1
						subs(i).text(it) = subs(i).text(it-1)
					Next

					subs(i).text(ti+1) = "" ' this line has been moved already, so blank it
					Dim As Integer splitpoint

					For it As Integer = 1 To Len(subs(i).text(ti))
						If it > Len(subs(i).text(ti)) /2 Then
							'We are at a point greater than 50%, time to look for a split point
							'This way the split doesn't cut a word in half
							If Left(Right(subs(i).text(ti),it),1) = " " Then
								subs(i).text(ti+1) = Right(subs(i).text(ti),it)
								splitpoint = it 'so we know where to split the other half
								Exit For
							End If
						EndIf
					Next

					subs(i).text(ti) = Left(subs(i).text(ti),  Len(subs(i).text(ti))-splitpoint  ) 'split the other half
				EndIf
			Next
			Dim As Integer numsubs
			For ti As Integer = 1 To 10
				If subfont.get_text_width(subs(i).text(ti)) > widepoint Then
					widepoint = subfont.get_text_width(subs(i).text(ti)) 'Widepoint = pixel width of widest line for the subtitle box size
				EndIf
				If subs(i).text(ti) = "" Then
					numsubs = ti-1
					subtop = (screeny-40) - (30*ti) 'Computet the top of the whole subtitle set. We start higher the more subs there are.
					Exit for
				EndIf
			Next
			widepoint+=20 ' so it extends a little bit beyond the edges
			If subs_mode = 2 Then 'Create the shadowbox the subs go in in mode 2
				Dim As fb.image Ptr subback = ImageCreate(widepoint,(numsubs*30)+20,black)
				Put(screenx/2 - widepoint/2,subtop),subback,Alpha, 160
				ImageDestroy(subback)
			End If
			for ti as integer = 1 to 10 'Actually print the subs, x centered
				cprint subtop + (ti*30),subs(i).text(ti), 2
			Next
			fontsize = fonts
			subfont.set_size fontsize
		EndIf
		if subs(i).text(1) = "" then exit For 'There should be no blank lines but this line may be dubious
	Next
End Sub

Sub debug_info
	Dim As String temp
	Draw String (10,10), "Debug infos:                            "
	Draw String (10,20), "________________________________________"
	Draw String (10,40), "Video position: " & Left(Str(videopos),5)
	Draw String (10,50), "Audio position: " & Left(Str(audiopos),5)
	Draw String (10,60), "Out of sync by: " & Left(Str(abs(videopos-audiopos)),5),IIf(Abs(videopos-audiopos) >.5,red,green)
'	Draw String (10,70), "Video target FPS: " & avir->fps
	If mode = "Audio" Then
		temp = "Actual FPS:" & Left(Str(frames / (Timer-updtime)),5)
	Else
		temp = "Actual FPS:" & Left(Str(frames_rendered/ audiopos),5)
	End If
	Draw String (10,80), temp
	Draw String (10,90), "Frames skipped: " & frames_skipped
'	Draw String (10,100), "Percentage of frames rendered: " & Left(Str((frames_rendered/avir->currentframe)*100),5)
	Draw String (10,110), "Zoomy: " & zoom_mult & "x"
	Draw String (10,120), "Video X offset: " & vid_x_offset
	Draw String (10,130), "Video Y offset: " & vid_y_offset
	temp = "Subtitles: "
	If subs_mode = 1 Then
		temp & = "Normal"
	ElseIf subs_mode = 2 Then
		temp & = "In shaded box"
	Else
		temp & = "Off"
	EndIf
	Draw String (10,140), temp
	Draw String (10,150), "Subtitle delay: " & suboffset
	temp = "Renderer: "
	If renderer = 1 Then temp & = "Nearest Neighbor"
	If renderer = 2 Then temp & = "Bilinear"
	Draw String (10,160), temp
	Draw String (10,170), "Mouse X: " & Str(mousex)
	Draw String (10,180), "Mouse Y: " & Str(mousey)
End Sub

Sub fillpix
	
    #Undef black
    #Define black  &hFF000000
    Dim As Integer w, h, bypp, pitch
    Dim buffer As UInteger Ptr
    Dim szbuff As UInteger

       ScreenInfo w, h, , bypp, pitch
       
       szbuff = w*h*4  

       buffer = ScreenPtr()
     
       ScreenLock()
       
       Asm
         
          pushad
         
          mov   ecx,[szbuff] '# of pixels
          mov   eax,[buffer] 'pointer to pixels

    begin:

          mov   ebx,[eax]   'load 32-bit pixel value
          cmp   ebx,black   'compare to black
          jne   nofill
         
          mov   ebx,[eax+4] 'Get pixel next door
          mov   [eax],ebx   'copy pixel to black position

    nofill:

          Add   eax, 4      'increment pixel pointer
          Sub   ecx,4      
          jecxz done
          jmp   begin
         
    done:
         
          popad
               
       End Asm
       
       ScreenUnLock()
       #Undef black
       #Define black RGB(0,0,0)

    End Sub