
'Levenshtein Distance Algorithm for FreeBASIC
'Based on the C implementation of Lorenzo Seidenari here: http://www.merriampark.com/ldc.htm
'This code is assumed to be available under the Public Domain.

Declare Function levenshtein_distance( s As String, t As String ) As Integer
Declare Function lev_minimum( a As Integer, b As Integer, c As Integer ) As Integer


Function levenshtein_distance( s As String, t As String ) As Integer

Dim As Integer k, i, j, n, m, cost, distance
Dim As Integer Ptr d

n = Len(s)
m = Len(t)

If (n <> 0) And (m <> 0) Then
   d = allocate( sizeof(Integer) * (m+1) * (n+1) )
   m += 1
   n += 1
   k = 0

   While k < n
      d[k]=k
      k += 1
   Wend

   k = 0
   While k < m
      d[k*n]=k
      k += 1
   Wend

   i = 1
   While i < n
      j = 1

      While j<m
         If (s[i-1] = t[j-1]) Then
            cost = 0

         Else
            cost = 1

         End If

         d[j*n+i] = lev_minimum(d[(j-1)*n+i]+1, d[j*n+i-1]+1, d[(j-1)*n+i-1]+cost)

         j += 1
      Wend

      i += 1
   Wend

   distance = d[n*m-1]
   deallocate d

   Return distance

Else
   Return -1

End If

End Function

Function lev_minimum( a As Integer, b As Integer, c As Integer ) As Integer

var min_ = a

If (b<min_) Then min_ = b
If (c<min_) Then min_ = c

Return min_

End Function

 