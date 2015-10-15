Dim As Integer px2
Declare Function pBar(pVal1 As Integer, pVal2 As Integer, pX As Integer, pY As Integer, pSize As Integer = 200, rgbv As UInteger = RGB(255,255,255), alph As UInteger = 255) As Integer
Declare Function Rounds(rNum As Double) As Integer
Declare Function Box(box_X As Integer, box_Y As Integer, box_Width As Integer, box_Height As Integer, rgbv As UInteger = RGB(255,255,255)) As Integer

'Function to round numbers.
Function Rounds(rNum As Double) As Integer
	If rNum - Fix(rNum) < .5 Then rNum = Fix(rNum) Else rNum = Fix(rNum)+1
	Return rNum
End Function


Function pBar(pVal1 As Integer, pVal2 As Integer, pX As Integer, pY As Integer, pSize As Integer = 200, rgbv As UInteger = RGB(255,255,255), alph As UInteger = 255) As Integer
	Dim As Double pX2
	Box(pX, pY, pSize, 20, RGB(alph,alph,alph))
'	pX2 = (Round(( (pSize/100) * ((pVal2/pVal1)*100) )+pX))-1
	If px2 > px Then
	For i As Integer = 1 To 19
		Line(px2, py+i)-(px+1, py+i), RGBv
	Next
	End if
	Return 1
End Function

'Function to draw a box to the screen.
Function Box(box_X As Integer, box_Y As Integer, box_Width As Integer, box_Height As Integer, rgbv As UInteger = RGB(255,255,255)) As Integer
	Line (box_X, box_Y)-(box_X+box_Width, box_Y), rgbv
	Line (box_X+box_Width, box_Y)-(box_X+box_Width, box_Y+box_Height), rgbv
	Line (box_X+box_Width, box_Y+box_Height)-(box_X, box_Y+box_Height), rgbv
	Line (box_X, box_Y+box_Height)-(box_X, box_Y), rgbv
	Return 1
End Function
