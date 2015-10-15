'mp3 tag reading functions
Declare Function getmp3tag(searchtag As String, fn As String) As String
Declare Function getalbumart(Byref fn As String, ByVal sz As UInteger) As Any Ptr


Function getmp3tag(searchtag As String, fn As String) As String
	'so we can avoid having the user need TALB for album, TIT2 for title etc, although they are accepted
	Dim As Integer skip, offset' in order to read certain things right
	Dim As UInteger sig_to_find, count, fnum, maxcheck = 100000
	dim as UShort tag_length
	Dim As UShort unitest, mp3frametest
	Dim As String tagdata


	Select Case UCase(searchtag)
		Case "HEADER", "ID3"
			searchtag = "ID3" & Chr(&h03)
			
		Case "TITLE", "TIT2"
			searchtag = "TIT2"

		Case "ARTIST", "TPE1"
			searchtag = "TPE1"

		Case "ALBUM", "TALB"
			searchtag = "TALB"

		Case "COMMENT", "COMM"
			searchtag = "COMM"

		Case "COPYRIGHT", "TCOP"
			searchtag = "TCOP"

		Case "COMPOSER", "TCOM"
			searchtag = "TCOM"

		Case "BEATS PER MINUTE", "BPM", "TPBM"
			searchtag = "TBPM"

		Case "PUBLISHER", "TPUB"
			searchtag = "TPUB"

		Case "URL", "WXXX"
			searchtag = "WXXX"

		Case "PLAY COUNT" "PCNT"
			searchtag = "PCNT"

		Case "GENRE", "TCON"
			searchtag = "TCON"

		Case "ENCODER", "TENC"
			searchtag = "TENC"

		Case "TRACK", "TRACK NUMBER", "TRCK"
			searchtag = "TRCK"

		Case "YEAR", "TYER"
			searchtag = "TYER"
		
		'Special, in this case we will return the datasize if present, or "-1" if no art
		Case "PICTURE", "APIC"
			searchtag = "APIC"
			'Not implemented yet!

		Case Else
			'Tag may be invalid, but search anyway, there are MANY tags, and we have error checking

	End Select

	fnum = FreeFile
	Open fn For Binary Access Read As #fnum
	If Lof(fnum) < maxcheck Then maxcheck = Lof(fnum)
	For count = 0 to maxcheck Step 1
		Get #fnum, count, sig_to_find
		If sig_to_find = Cvi(searchtag) Then
			
			If searchtag = "ID3" & Chr(&h03) Then
				Close #fnum
				Return "1" 'Because there is no data here, we were just checking for the ID3 header
			EndIf
					'test for unicode
			Get #fnum, count+11, unitest
			
			If unitest = &hFEFF Then 'unicode string
				skip = 4
				offset = 13
				
			Else 'not unicode string
				skip = 0
				offset = 10
				
			EndIf
			
			Get #fnum, count +7, tag_length 'XXXXYYYZZ Where XXXX is the TAG, YYY is flags or something, ZZ is size

			If tag_length-skip < 1 Then
				Close #fnum
				Return "ERROR" 'In case of bad things
			EndIf
			
			Dim As Byte dataget(1 To tag_length-skip)
			Get #fnum, count+offset, dataget()
			
			For i As Integer = 1 To tag_length - skip
				If dataget(i) <> 0 Then tagdata + = Chr(dataget(i)) 'remove null spaces from ASCII data in UNICODE string
			Next
		End If
			If tagdata <> "" then exit For ' stop searching!
	Next
	Close #fnum
	
	If Len(tagdata) = 0 Then 
		'If the tag was just not found or had no data then "----"
		tagdata = "----"
	EndIf
	Return tagdata
End Function


Function getalbumart(Byref fn As String, ByVal sz As UInteger) As Any Ptr
	Dim As UInteger sigfind
	Dim As Integer count, infile, outfile
	infile = FreeFile
	Open fn For Binary Access Read As #infile
	For count = 1 To Lof(infile)
		Get #infile, count, sigfind
		'search for APIC sig in MP3 = "Attached Picture"
		If sigfind = Cvi("APIC") Then
			For count = count To Lof(infile) Step 1
				Get #infile, count, sigfind
				'JPEG
				If sigfind = &hE0FFD8FF Then
					outfile = FreeFile
					Open ExePath & "/out.jpg" For Binary As #outfile
					Dim As Byte fullpic(1 To sz-13)
					Get #infile, count, fullpic()
					Put #outfile, 1, fullpic()
					Close #infile
					Close #outfile
					Dim As Any Ptr img = freeimage_load_fb(ExePath & "/out.jpg", TRUE)
					Kill(ExePath & "/out.jpg")
					Return img
					'PNG
				ElseIf sigfind = Cvi(!"\&h89PNG") Then
					outfile = FreeFile
					Open ExePath & "/out.png" For Binary As #outfile
					Dim As Byte fullpic(1 To sz)
					Get #infile, count, fullpic()
					Put #outfile, 1, fullpic()
					Close #infile
					Close #outfile
					Dim As Any Ptr img = freeimage_load_fb(ExePath & "/out.png", TRUE)
					Kill(ExePath & "/out.png")
					Return img
				EndIf
			Next
		EndIf
	Next
End Function
