/'--------------------------------------------------------------------
                          AVI Reader Object              
                  Copyright Vincent DeCampo 2008      

   This program is free software: you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License
as published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Last Updated: 11/27/2008
--------------------------------------------------------------------
Note: Does not support sound streams, only video.
--------------------------------------------------------------------
ToDo: Add Sound stream support
--------------------------------------------------------------------'/

#Include Once "win\vfw.bi"
#Include Once "crt.bi"

#define FCC(c) *(cptr(Uinteger Ptr,@##c))
#Define pCAST(p)  Cast(Any Ptr,p)

Type _AVI_READER
   Declare Constructor  (ByVal szFile As ZString Ptr)
   Declare Destructor   ()
   Declare Function GetNextFrame() As HANDLE
   Declare Function SetFirstFrame() As HANDLE
   Declare Sub      RGB_RGBA(temp1 As Any Ptr,temp2 As Any Ptr, nPixels As Integer)
   Declare Sub      FlipBuffer(pbuff As HANDLE)
Private:
   AVIFile      As PAVIFILE Ptr
   AVIStream    As PAVISTREAM Ptr
   fileInfo     As AVIFILEINFOA 
   streamInfo   As AVISTREAMINFOA
   outBI        As BITMAPINFOHEADER
   pGetFrameObj As PGETFRAME
Public:
   SizeX        As Integer
   SizeY        As Integer
   Pitch        As Integer
   numFrames    As Integer
   firstframe   As Integer
   currentframe As Integer
   fps          As Single
   tpf          As Single
End Type

Constructor _AVI_READER (ByVal szFile As ZString Ptr)
Dim ErrOut As Integer = 0   

   Do
      
      AVIFileInit
      '
      ErrOut = IIf (AVIFileOpen(Cast(PAVIFILE Ptr,@AVIFile), szFile, OF_SHARE_DENY_WRITE, 0)=AVIERR_OK,0,1)
      If ErrOut Then Exit Do
      '
      ErrOut = IIF (AVIFileGetStream(Cast(PAVIFILE,AVIFile),Cast(PAVISTREAM Ptr,@AVIStream), FCC("vids"), 0)=AVIERR_OK,0,1)
      If ErrOut Then Exit Do
      '
      this.firstFrame = AVIStreamStart(Cast(PAVISTREAM,AVIStream))
      ErrOut = IIF (firstFrame = -1,1,0)
      If ErrOut Then Exit Do
      '
      numFrames = AVIStreamLength(Cast(PAVISTREAM,AVIStream))
      ErrOut = IIf (numFrames = -1,1,0)
      If ErrOut Then Exit Do
      '
      ErrOut = IIf (AVIFileInfo(Cast(PAVIFILE,AVIFile), @fileInfo, SizeOf(fileInfo))=AVIERR_OK,0,1)
      If ErrOut Then Exit Do
      '
      ErrOut = IIf (AVIStreamInfo(Cast(PAVISTREAM,AVIStream), @streamInfo, SizeOf(streamInfo))=AVIERR_OK,0,1)
      If ErrOut Then Exit Do

      With outBI
          .biSize = SizeOf(outBI)
          .biBitCount = 24
          .biClrImportant = 0
          .biClrUsed = 0
          .biCompression = BI_RGB
          .biHeight = streamInfo.rcFrame.bottom - streamInfo.rcFrame.top
          .biPlanes = 1
          .biWidth = streamInfo.rcFrame.right - streamInfo.rcFrame.left
          .biXPelsPerMeter = 0
          .biYPelsPerMeter = 0
          .biSizeImage = (((.biWidth * 3) + 3) And &HFFFC) * .biHeight
      End With
      
      this.SizeX = outBI.biWidth
      this.SizeY = outBI.biHeight
      this.pitch = outBI.biWidth * (outBI.biBitCount/8)
      
      this.pGetFrameObj = AVIStreamGetFrameOpen(Cast(PAVISTREAM,AVIStream), @outBI) 
      this.currentframe = this.firstframe
      this.fps = streaminfo.dwrate / streaminfo.dwscale
      this.tpf = 1000/this.fps

   Loop Until (1)

   If ErrOut Then
      If AVIStream <> 0 Then
          AVIStreamRelease(Cast(PAVISTREAM,AVIStream)) 'closes video stream
      End If
      If AVIFile <> 0 Then
         AVIFileRelease(Cast(PAVIFILE,AVIFile)) ' closes the file
      End If
   EndIf
    
End Constructor

Destructor _AVI_READER ()

   If pGetFrameObj<>0 Then
      AVIStreamGetFrameClose(pGetFrameObj) 'deallocates the GetFrame resources
   End If
   
   If AVIStream <> 0 Then
       AVIStreamRelease(Cast(PAVISTREAM,AVIStream)) 'closes video stream
   End If
   
   If AVIFile <> 0 Then
      AVIFileRelease(Cast(PAVIFILE,AVIFile)) ' closes the file
   End If
   
   AVIFileExit
    
End Destructor

Function _AVI_READER.SetFirstFrame() As HANDLE

   this.currentframe = this.firstframe
   Return 0
   
End Function

Function _AVI_READER.GetNextFrame() As HANDLE
Dim cf As Integer = this.currentframe
   
   this.currentframe += 1
   If this.currentframe > (this.numFrames-1) Then
      this.currentframe = this.firstframe
   EndIf

   Return AVIStreamGetFrame(this.pGetFrameObj, cf) 

End Function

Sub _AVI_READER.RGB_RGBA(temp1 As Any Ptr,temp2 As Any Ptr, nPixels As Integer)

   Asm
       pushad
       mov eax, [temp1]
       mov edx, [temp2]
       mov ecx, [nPixels]
   
   RGBA_Loop:
   
       mov ebx, 0
       mov bh, [eax+2]
       mov bl, [eax+1]
       Rol ebx, 8
       mov bl, [eax]
       mov [edx], ebx
   
       add edx, 4
       add eax, 3
       Sub ecx, 1                  
       
       jecxz RGBA_Done
       jmp RGBA_Loop
   
   RGBA_Done:
   
       popad
          
   End Asm                  

End Sub

Sub _AVI_READER.FlipBuffer(pdata As Any Ptr)
Dim As Any Ptr tline = Allocate(this.pitch)
Dim As Integer lines = this.SizeY
Dim As Any Ptr src,dst
   
   For scanline As Integer = 0 To (lines\2)-1
      
      src = pdata+(lines-scanline-1)*this.pitch
      dst = pdata+(scanline*pitch)
      
      memcpy tline, src,   this.pitch
      memcpy src,   dst,   this.pitch      
      memcpy dst,   tline, this.pitch
      
   Next
   
   DeAllocate(tline)

End Sub

