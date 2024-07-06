* user 1 1 for space !!
* ptot= I * J  : 3 3 
* run mplot_scr_factor_decomp_91_horiz.gs 5 2 => 3 lines, 2 columns, order i fix (allj)
**************************************************
* panels_demo.gs 
* 
* This script demonstrates the use of panels.gsf, a dynamically 
* loaded script function that sets up the virtual page commands 
* for a multi-panel plot. 
* 
* Written by JMA March 2001
*

*function main(args)
*var=Flo
*ystart=1976
*yend=2005
*uslice='2021-2030'
*scen=historical
*uscen=rcp85
*kode=576133
*nz=1890
*nint=378
*cmod=ECE
*dpr=PRINT


  rc = gsfallow("on")
  if (args='') 
    say 'Two arguments are required: the # of rows and # of columns'
    return 
  else 
    nrows = subwrd(args,1)
    ncols = subwrd(args,2)
  endif
*
'set rgb 17   0   0 255'
'set rgb 18   20 20 255'
'set rgb 19   40 40 255'
'set rgb 20   60 60 255'
'set rgb 21   80 80 255'
'set rgb 22  90  90 255'
'set rgb 23 110 110 255'
'set rgb 24 130 130 255'
'set rgb 25 150 150 255'
'set rgb 26 165 165 255'
'set rgb 27 185 185 255'
'set rgb 28 200 200 255'
'set rgb 29 220 220 255'
* These are the RED shades
*'set rgb 30 255 220 220'
'set rgb 30 255 215 215'
'set rgb 31 255 210 210'
'set rgb 32 255 200 200'
'set rgb 33 255 190 190'
'set rgb 34 255 180 180'
'set rgb 35 255 170 170'
'set rgb 36 255 160 160'
'set rgb 37 255 150 150'
'set rgb 38 255 140 140'
'set rgb 39 255 130 130'
'set rgb 40 255 120 120'
'set rgb 41 255 110 110'
'set rgb 42 255 100 100'
'set rgb 43 255  90 90 '
'set rgb 44 255  80 80 '
'set rgb 45 255  70  70'
'set rgb 46 255  60  60'
'set rgb 47 255  50 50'
'set rgb 48 255  40  40'
'set rgb 49 255  30 30 '
'set rgb 50 255   0   0'

 
**************************
*


* These are the BLUE shades
'set rgb 16   0   0 255'
'set rgb 17  20  20 255'
'set rgb 18  40  40 255'
'set rgb 19  60  60 255'
'set rgb 20  70  70 255'
'set rgb 21  80  80 255'
'set rgb 22 100 100 255'
'set rgb 23 100 100 255'
'set rgb 24 120 120 255'
'set rgb 25 140 140 255'
'set rgb 26 150 140 255'
'set rgb 27 160 160 255'
'set rgb 28 170 180 255'
'set rgb 29 180 180 255'
'set rgb 30 190 200 255'
'set rgb 31 200 200 255'
'set rgb 32 210 200 255'
*'set rgb 33 217 220 255'
'set rgb 33 220 220 255'

* These are the RED shades
'set rgb 39 255 240 240'
'set rgb 40 255 220 220'
'set rgb 41 255 200 200'
'set rgb 42 255 180 180'
'set rgb 43 255 160 160'
'set rgb 44 255 140 140'
'set rgb 45 255 120 120'
'set rgb 46 255 100 100'
'set rgb 47 255  80  80'
'set rgb 48 255  60  60'
'set rgb 49 255  70  70'
'set rgb 50 255  40  40'
'set rgb 51 255  20  20'
'set rgb 52 255   0   0'

'set rgb 53 225   60   60'
'set rgb 54 180   30   30'

*********************************

  panels(args)
  p = 1
  ptot = nrows * ncols
  'set mproj scaled'

* Loop through each panel and draw a plot
*  while (p <= ptot)
**d**************************************************************
*d**********************************************
'open o1_var1_'var'.ctl'

******************************************
    p = 1
while (p<=30)
    _vpg.p
'set t 'p

yy=ystart+p-1

'set xlopts 1 6 0.16'
'set ylopts 1 6 0.16'
'set xaxis 1 'nz' 'nint
'set x 1'
'set z 1 'nz
'set xyrev on'
'set grads off'
'd har'

'set x 5'
'set z 1 'nz
'set xyrev on'
'set grads off'
'd har'

'set x 9'
'set z 1 'nz
'set xyrev on'
'set grads off'
'd har'

'draw title 'var', scen='scen', year:'yy', kode='kode
p=p+1
endwhile
'printim 'dpr'/'var'_'scen'_yy'yy'_'kode'.jpg white'

*****************************************************************
**************************************************
* panels.gsf
*
* This function evenly divides the real page into a given number of rows
* and columns then creates global variables that contain the 'set vpage'
* commands for each panel in the multi-panel plot.
*
* Usage: panels(rows cols)
*
* Written by JMA March 2001
*
function panels(args)

* Get arguments
  if (args='')
    say 'panels requires two arguments: the # of rows and # of columns'
    return
  else
    nrows = subwrd(args,1)
    ncols = subwrd(args,2)
  endif

* Get dimensions of the real page
  'query gxinfo'
  rec2  = sublin(result,2)
  xsize = subwrd(rec2,4)
  ysize = subwrd(rec2,6)

* Calculate coordinates of each vpage
  width  = xsize/ncols
  height = ysize/nrows
  row = 1
  col = 1
  panel = 1
  while (row <= nrows)
    yhi = ysize - (height * (row - 1))
    if (row = nrows)
      ylo = 0
    else
      ylo = yhi - height
    endif
    while (col <= ncols)
      xlo = width * (col - 1)
      xhi = xlo + width
      _vpg.panel = 'set vpage 'xlo'  'xhi'  'ylo'  'yhi
      panel = panel + 1
      col = col + 1

 endwhile
    col = 1
    row = row + 1
  endwhile
  return

* THE END *

