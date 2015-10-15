Type _DriveList
	Public:
	Declare Constructor()
	'
	As Integer  count
	As String   drive(10)
	As Integer  dType(10)
	Private:
	As Zstring*255 drives
End Type
Constructor _DriveList
Dim As Integer buff,idx

buff = GetLogicalDriveStrings(255,@drives)

idx=0
For a As Integer = 0 To buff-1
	If drives[a]=0 Then
		this.dtype(idx)=GetDriveType(this.drive(idx))
		idx+=1
		If idx>10 Then Exit For
	Else
		drive(idx)+=Chr(drives[a])
	Endif
Next

this.count = idx

End Constructor

