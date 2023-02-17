;LandXML for Autocad - importer, creator and exporter of Victorian based LandXML data
;Copyright (C) 2015  Phillip Nixon

;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    any later version.

;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.

;    To view a copy of the GNU General Public License see <http://www.gnu.org/licenses/>.

;THE BEER-WARE LICENSE" (Revision 42):
; Phillip Nixon wrote this file. As long as you retain this notice you can do whatever you want with this stuff.
;If we meet some day, and you think this stuff is worth it, you can buy me a beer in return.

;Edited from version 1.8.11 of NSW version
;Revision 1.0 – Beta
;Revision 1.1    Reserves added to road creators
;               Fixed XAM not assigning text to Admin Sheet layer
;               Checked for EAS while adding link parcels to easement
;               Fixed XAS reference number prompt
;Revision 1.2    Fixed XAS reference number prompt
;               Fixed problem with creating, importing and exporting restrictions
;               Fixed problem with two point irregular lines
;               Changes to topo and sideshot exporting pntsurv styles
;               Other minor bug fixes
;Revision 1.2.1-Change CG point importer to deal with stringer different linefeeds
;Revision 1.2.2-Irregualr Duplicate point remover added
;Revision 1.2.3-Changed so XMO is not exporting all monuments in job
;Revision 1.2.4-Made head of power and purpose all captial and uncapitals on xo
;              -Fixed problem with multiple hop export
;              -Made allowance for bearing only lines on xin (not on xino yet)
;Revision 1.2.5-Improved importer to deal with non-linefed files
;              -Added cardinal end letter checker
;              -Median Face changed to median on xin and xout
;              -Planfeatures importer modified to align better with Vic recipe
;              -Fix problem with cardinals calcing 0' minutes
;Revision 1.3  -Set alerts for easement uses that are too long
;              -Adjusted easement width mtext box
;              -Fixed problem with single line restrictions
;              -Updates to irregular line importer
;              -Fix problem with easement widths not going in right place on xin
;              -Added AD, COMP, DERI and MEAS to all reduced obs
;              -Refinements to XRT and XPU
;              -Added XCD function for checking check digits
;Revision 1.4  -Added XINOA function to automatically load xmls from specified file locations
;              -Added latlong to MGA converter for early release back captured data for xin
;              -Default scale set to 200
;Revision 1.4.1-Made estimated distances red
;Revision 1.4.2-XMT fixed to work in UCS
;              -Added loop checker drawing XLCD command
;Revision 1.4.3-Expanded XRT
;              -Removed default text height
;              -Fixed version concatenator on export
;              -Changed purposelist to be lower case
;              -Added searching for Suplementary AFR's to XINOA


(setq version "1.4.3")


(REGAPP "LANDXML")

;Set up page and variables for xml work
(SETVAR "CMDECHO" 0)
(SETVAR "ATTDIA" 0)
(SETVAR "ATTREQ" 1)
(SETVAR "PLINETYPE" 2) ;CONVERT TO POLYLINES NOT 2D POLYLINES WHEN PEDITING
(SETVAR "PEDITACCEPT" 0);DONT PROMT FOR POLYLINE CHANGES

(setq textstyle (getvar "textstyle"))
(SETQ textfont (ENTGET (tblobjname "style" textstyle)))
(setq theElist (subst (cons 40 0)(assoc 40 theElist) textfont))
(entmod theElist)



;Check to see if scale and rounding values are stored in drawing
(IF (= (SETQ MAXLEN1 (VLAX-LDATA-GET "LXML4AC" "MAXLEN1" )) nil) (PROGN   (setq MAXLEN1 1)  (VLAX-LDATA-PUT "LXML4AC" "MAXLEN1" MAXLEN1)   )  )
(IF (= (SETQ maxlen2 (VLAX-LDATA-GET "LXML4AC" "maxlen2" )) nil) (PROGN   (setq maxlen2 10000)  (VLAX-LDATA-PUT "LXML4AC" "maxlen2" maxlen2)   )  )
(IF (= (setq brnd1   (VLAX-LDATA-GET "LXML4AC" "brnd1" )) nil) (PROGN   (setq brnd1 60)  (VLAX-LDATA-PUT "LXML4AC" "brnd1" brnd1)   )  )
(IF (= (setq brnd2   (VLAX-LDATA-GET "LXML4AC" "brnd2" )) nil) (PROGN   (setq brnd2 5)  (VLAX-LDATA-PUT "LXML4AC" "brnd2" brnd2)   )  )
(IF (= (setq brnd3   (VLAX-LDATA-GET "LXML4AC" "brnd3" )) nil) (PROGN   (setq brnd3 1)  (VLAX-LDATA-PUT "LXML4AC" "brnd3" brnd3)   )  )
(IF (= (setq distmax1 (VLAX-LDATA-GET "LXML4AC" "distmax1" )) nil) (PROGN   (setq distmax1 1)  (VLAX-LDATA-PUT "LXML4AC" "distmax1" distmax1)   )  )
(IF (= (setq distmax2 (VLAX-LDATA-GET "LXML4AC" "distmax2" )) nil) (PROGN   (setq distmax2 1)  (VLAX-LDATA-PUT "LXML4AC" "distmax2" distmax2)   )  )
(IF (= (setq drnd1 (VLAX-LDATA-GET "LXML4AC" "drnd1" )) nil) (PROGN   (setq drnd1 0.005)  (VLAX-LDATA-PUT "LXML4AC" "drnd1" drnd1)   )  )
(IF (= (setq drnd2 (VLAX-LDATA-GET "LXML4AC" "drnd2" )) nil) (PROGN   (setq drnd2 0.005)  (VLAX-LDATA-PUT "LXML4AC" "drnd2" drnd2)   )  )
(IF (= (setq drnd3 (VLAX-LDATA-GET "LXML4AC" "drnd3" )) nil) (PROGN   (setq drnd3 0.005)  (VLAX-LDATA-PUT "LXML4AC" "drnd3" drnd3)   )  )
(IF (= (setq qround (VLAX-LDATA-GET "LXML4AC" "qround" )) nil) (PROGN   (setq qround "NO")  (VLAX-LDATA-PUT "LXML4AC" "qround" qround)   )  )
(IF (= (setq scale (VLAX-LDATA-GET "LXML4AC" "scale" )) nil) (PROGN   (setq scale 200)  (VLAX-LDATA-PUT "LXML4AC" "scale" scale)   )  )
(IF (= (setq th    (VLAX-LDATA-GET "LXML4AC" "TH" )) nil) (PROGN   (setq TH 1.25)  (VLAX-LDATA-PUT "LXML4AC" "TH" TH)   )  )
(IF (= (setq ATHR  (VLAX-LDATA-GET "LXML4AC" "ATHR" )) nil) (PROGN   (setq ATHR "Y")  (VLAX-LDATA-PUT "LXML4AC" "ATHR" ATHR)   )  )

(setvar "celtscale" (/ (/ scale 100) 2))
(if (= setupdone nil)(progn
(setq prevlayer (getvar "CLAYER"))
(SETQ QROUND "NO")


;LOAD LINETYPES
(setq expertlevel (getvar "expert"))
(setvar "expert" 3)
(COMMAND "-LINETYPE" "L" "*" "LANDXML_VF" "" )
 (setvar "expert" expertlevel)




;(if (= textfontext "ttf") (COMMAND "STYLE" "" "" "0" "" "" "" "" ))
;(if (= textfontext "shx") (COMMAND "STYLE" "" "" "0" "" "" "" "" "" ))

      (setvar "cannoscale" "1:1")
(command "-scalelistedit" "d" "*" "e")

      
;(command "layer" "m" "Road" "c" "red" "" "lw" "0.5" "" "")
(command "layer" "m" "Boundary" "c" "blue" "" "lw" "0.5" "" "")
;(command "layer" "m" "Connection" "c" "cyan" "" "lw" "0.25" "" "l" "EASEMENT" "" "")
(command "layer" "m" "Easement" "c" "8" "" "lw" "0.25" "" "l" "EASEMENT" "" "")
;(command "layer" "m" "PM Connection" "c" "193" "" "lw" "0.25" "" "l" "PM_CONNECTION" "" "")
;(command "layer" "m" "RM Connection" "c" "yellow" "" "p" "n" "" "")
(command "layer" "m" "Monument" "c" "magenta" ""  "p" "n" "" "")
(command "layer" "m" "Lot Definitions" "c" "green" "" "p" "n" "" "lw" "0.7" "" "")
(command "layer" "m" "Adjoining Boundary" "c" "white" "" "lw" "0.25" "" "")
(command "layer" "m" "PM" "p" "n" "" "")
(command "layer" "m" "Datum Points" "p" "n" "" "")
(command "layer" "m" "Drafting" "c" "white" "" "lw" "0.25" "" "")
(command "layer" "m" "Drafting AFR" "c" "cyan" "" "lw" "0.25" "" "")
(command "layer" "m" "CG Points" "c" "white" "" "lw" "0.25" "" "p" "n" "" "")
(command "layer" "m" "Admin Sheet" "c" "white" "" "lw" "0.25" "" "")
(command "layer" "m" "Occupation Building Return" "c" "white" "" "lw" "0.25" "" "L" "BUILDING" "" "")
(command "layer" "m" "Occupation Walls" "c" "white" "" "lw" "0.25" "" "L" "WALL" "" "")
(command "layer" "m" "Occupation Timber Walls" "c" "white" "" "lw" "0.25" "" "L" "WALL" "" "")
(command "layer" "m" "Occupation Fences" "c" "8" "" "lw" "0.25" "" "L" "FENCE" "" "")
(command "layer" "m" "Occupation Kerbs" "c" "9" "" "lw" "0.25" "" "L" "KERB" "" "")
(command "layer" "m" "Occupation Gate" "c" "9" "" "lw" "0.25" "" "L" "KERB" "" "")
(command "layer" "m" "Occupation Centreline" "c" "9" "" "lw" "0.25" "" "L" "KERB" "" "")
(command "layer" "m" "Occupation Not Fenced" "c" "9" "" "lw" "0.25" "" "L" "KERB" "" "p" "n" "" "")
(command "layer" "m" "Occupation Railway" "c" "9" "" "lw" "0.25" "" "L" "KERB" "" "")
(command "layer" "m" "Occupation Rockwall" "c" "9" "" "lw" "0.25" "" "L" "KERB" "" "")
(command "layer" "m" "Occupation Hedge" "c" "9" "" "lw" "0.25" "" "L" "KERB" "" "")
(command "layer" "m" "Occupation Other" "c" "9" "" "lw" "0.25" "" "L" "KERB" "" "")

(command "layer" "m" "Occupations" "c" "green" "" "p" "n" "" "")
(command "layer" "m" "Irregular Boundary" "c" "140" "" "lw" "0.5" "" "")
(command "layer" "m" "Irregular Right Lines" "c" "9" "" "p" "n" "" "")

;VIC
;(command "layer" "m" "Normal" "c" "blue" "" "lw" "0.5" "" "")
(command "layer" "m" "Boundary Extinguished" "c" "red" "" "lw" "0.5" "" "")
(command "layer" "m" "Boundary Wall Interior" "c" "23" "" "lw" "1" "" "l" "STRUCTURE_INTERIOR" "" "")
(command "layer" "m" "Boundary Wall Exterior" "c" "23" "" "lw" "1" "" "l" "STRUCTURE_EXTERIOR" "" "")
(command "layer" "m" "Boundary Wall Median" "c" "23" "" "lw" "1" "" "l" "STRUCTURE_MEDIAN" "" "")
(command "layer" "m" "Boundary Wall Other" "c" "23" "" "lw" "1" "" "")
(command "layer" "m" "Traverse" "c" "cyan" "" "lw" "0.25" "" "l" "TRAVERSE" "" "")
(command "layer" "m" "Sideshot" "c" "yellow" ""  "lw" "0.25" "" "l" "EASEMENT" "" "")
(command "layer" "m" "Topo" "c" "123" ""  "lw" "0.25" "" "l" "EASEMENT" "" "")


;rounding defaults
(setq maxlen1 30)
(setq maxlen2 200)
(setq brnd1 60);less than maxlen1
(setq brnd2 10);from maxlen1 to maxlen2
(setq brnd3 10);greater than maxlen2
(setq distmax1 500)
(setq distmax2 5000)
(setq drnd1 0.005);less than distmax1
(setq drnd2 0.01);from distmax1 to distmax2
(setq drnd3 0.1);greater than distmax2


(setvar "clayer" prevlayer )
(setvar "celtscale" 1)
(setvar "pdmode" 35)
(setvar "pdsize" 0.2)



;@@@@@@possibly add rm and pop defintion here
(setq setupdone 1)
));p&if

(setvar "osnapcoord" 1)
(setvar "angdir" 1)
(setvar "angbase" (* 0.5 pi))
(setvar "attreq" 1)
(setvar "dimzin" 8)
(SETQ UNITS "M")

;default values

(setq rmtype "G.I. Pipe")
(setq rmstate "Found")
(setq crmstate "Found")
(if (= lrmrefdp nil)(setq lrmrefdp ""))
(if ( = ped nil)(setq ped ""))
(if (= ATHR nil)(setq ATHR "Y"))
(setq prevowner "")





;checking lists
(setq rmtypelist (list "Bolt" "Chisel Cut" "Cross Head Nail" "Deep Driven Rod" "Drill Hole" "Drill Hole with Wings"
		       "Dumpy" "G.I. Nail" "Nail in Peg" "Nail in Rail" "Not Marked" "Peg" "Peg and Trench" "Pin"
		       "G.I. Pipe" "Plaque" "Rivet" "Rod" "Reference Tree" "Screw" "Spike" "Star Picket" "Survey Nail"
		       "Other" "SSM (Standard Survey Mark)" "Square Post" "Round Post" "Split Post" "Nail in Join" "Tree"

))

(setq rmconditionlist (list "Abandoned" "Damaged" "Destroyed" "Disturbed" "Found" "Leaning" "Loose" "Nipple Damaged" "Not Found"
"Not Used" "OK" "Origin" "Placed" "Plaque Missing" "Removed" "Replaced" "Suspect" "Unknown" "Unstable"))

(setq autoloadff "J:\\IE\\Admin\\03_Southern\\Survey\\03_CAD Standards Survey\\vicmap_cad_supply\\XML\\")
(SETQ autoloadlist (list
		     "ARARAT"
		     "BALLARAT"
                     "BANYULE"
		     "BASS COAST"
		     "BAW BAW"
		     "BAYSIDE"
		     "BOROONDARA"
		     "BRIMBANK"
		     "CAMPASPE"
		     "CARDINIA"
		     "CASEY"
		     "COLAC OTWAY"
		     "CORANGAMITE"
		     "DAREBIN"
		     "FRANKSTON"
		     "GLEN EIRA"
		     "GLENELG"
		     "GOLDEN PLAINS"
		     "GREATER BENDIGO"
		     "GREATER DANDENONG"
		     "GREATER GEELONG"
		     "GREATER SHEPPARTON"
		     "HOBSONS BAY"
		     "HORSHAM"
		     "HUME"
		     "KINGSTON"
		     "KNOX"
		     "LATROBE"
		     "LODDON"
		     "MACEDON RANGES"
		     "MANNINGHAM"
		     "MARIBYRNONG"
		     "MAROONDAH"
		     "MELBOURNE"
		     "MELTON"
		     "MILDURA"
		     "MITCHELL"
		     "MOIRA"
		     "MONASH"
		     "MOONEE VALLEY"
		     "MOORABOOL"
		     "MORELAND"
		     "MORNINGTON PENINSULA"
		     "MOUNT ALEXANDER"
		     "MOYNE"
		     "MURRINDINDI"
		     "NILLUMBIK"
		     "PORT PHILLIP"
		     "SOUTH GIPPSLAND"
		     "SOUTHERN GRAMPIANS"
		     "STONNINGTON"
		     "SURF COAST"
		     "SWAN HILL"
		     "WANGARATTA"
		     "WARRNAMBOOL"
		     "WELLINGTON"
		     "WHITEHORSE"
		     "WHITTLESEA"
		     "WODONGA"
		     "WYNDHAM"
		     "YARRA"
		     "YARRA RANGES"
			 
))


;end of page setup
(princ (strcat "\nLandxml for Autocad VIC Recipe Version " version ))
(princ "\nCommand List")
(princ "\nXSS - Set Drawing Scale")
(princ "\nXRD - Set Drawing to Round")
(princ "\nXMLC - Redefine Lot Centre Positions")

(princ "\nXTR - Traverse")
(princ "\nXTA - Arc Traverse")
(princ "\nXTC - Chainage Traverse")
(princ "\nXTS - Angle Traverse")
(princ "\nXCL - Create Lot")
(princ "\nXCE - Create Easement Lot")
(princ "\nXLE - Link Easement Geometry")
(princ "\nXCR - Create Road/Reserve Lot")
(princ "\nXCM - Create mark")
(princ "\nXPM - Create PM mark (PM or SSM)")
(princ "\nXDP - Create Datum Point")
(princ "\nXOC - Create occupation offset")
(princ "\nXOQ - Create Queenland style point occupation")
(princ "\nXCF - Create Flow arrow")
(princ "\nXAS - Create Admin Sheet")
(princ "\nXLA - Add Layout Sheet")
(princ "\nXCOC - Add Owners Corporation Schedule")

(princ "\nXAL- Assign line to XML")
(princ "\nXALN - Assign line with note")
(princ "\nXAA - Assign arc to XML")
(princ "\nXAAN - Assign arc with note")
(princ "\nXAP - Assign Polyline Lot to XML")
(princ "\nXAE - Assign Polyline Easement to XML")
(princ "\nXAR - Assign Polyline Road/Reserve to XML")
(princ "\nXJL - Assign Polyline to Adjoining Boundary")
(princ "\nXJR - Assign Polyline to Existing Road")
(princ "\nXAI - Assign Polyline as Irregular Boundary")
(princ "\nXAC - Assign Polyline as chainage")
(princ "\nXAO - Assign description to Occupation")
(princ "\nXAM - Assign multitext")
(princ "\nXAN - Assign note to line/arc")
(princ "\nXTI - Assign Title to lot")
(princ "\nXAD - Assign Address to lot")
(princ "\nXSL- Assign lines to Short Line Table")
(princ "\nXSC - Assign arcs to Short Line Table")
(princ "\nXOS - Offest line")
(princ "\nXSW - Swap text positions")
(princ "\nXSP - Spin text 180°")
(princ "\nXCB - Create brackets around text")
(princ "\nXMT - Create tick marks")
(princ "\nXRT - Recreate Text from Xdata")
(princ "\nXPU - Push text past line")
(princ "\nXDE - Edit xdata manually")
(princ "\nXMO - Export Monuments to CSV")
(princ "\nXAUD - Audit geomtry against Xdata")
(princ "\nXCD - Check Check Digits")
(princ "\nXLCD - Check loops on drawn geometry")

(princ "\nXIN - Import XML file")
(princ "\nXINO - Import XML file from observations")
(princ "\nXINOA - Autmatically Import XML file from observations")
(princ "\nXINS - Import simple XML file from observations")
(princ "\nXOUT - Export XML file")
(princ (strcat "\nLandxml for Autocad VIC Recipe Version " version ))


;-------------------------------------------------------------------SET SCALE AND AUTOROUND-------------------------------------
(defun C:XSS (/)
  (princ (strcat "\nCurrent Scale is 1:"(RTOS scale 2 0)))
(setq SCALE (getreal "\nType Scale 1:"))
  (setq TH (* 2.5 (/ scale 1000 )))
  (setvar "celtscale" (/ (/ scale 100) 2))
  (setq ATHR (getstring "\nAutomatically Reduce Text Height? (Y/N):"))
  (if (or (= ATHR "")(= ATHR "y")(= ATHR "YES")(= ATHR "Yes")(= ATHR "yes"))(setq ATHR "Y"))
    (VLAX-LDATA-PUT "LXML4AC" "scale" scale)
  (VLAX-LDATA-PUT "LXML4AC" "TH" TH)
  (VLAX-LDATA-PUT "LXML4AC" "ATHR" ATHR) 
)


(defun C:XRD (/)
  (SETQ QROUND (GETSTRING "\nDo you want to Autoround when using Assigning Tools?(Y/N):"))
  (IF (OR (= QROUND "Y") (= QROUND "y"))(progn (SETQ QROUND "YES")
					  (princ (strcat "\nCurrent Values \nBearings \nLess than " (rtos maxlen1 2 3) "m round to " (rtos brnd1 2 0) " seconds"
							 "\nFrom " (rtos maxlen1 2 3) "m to " (rtos maxlen2 2 3) "m round to " (rtos brnd2 2 0) " seconds"
							 "\nGreater than " (rtos maxlen2 2 3) "m round to " (rtos brnd3 2 0) " seconds"
							 "\nDistance \nLess than " (rtos distmax1  2 3) "m round to " (rtos drnd1 2 3)
							 "\nFrom " (rtos distmax1 2 3) "m to " (rtos distmax2 2 3) "m round to " (rtos drnd2 2 3)
							 "\nGreater than " (rtos distmax2 2 3) "m round to " (rtos drnd3 2 3)))
					  (setq cvals (getstring "\nDo you wish to change these values?(Y/N):"))
					   (IF (OR (= cvals "Y") (= cvals "y"))(changerounding))
					  )
  (IF (OR (= QROUND "N") (= QROUND "n"))(SETQ QROUND "NO"))
  )
  (VLAX-LDATA-PUT "LXML4AC" "qround" qround) 
  )


  (defun changerounding (/)
    (princ "\nBearings")
    (setq maxlen1 (getreal "\nLess than:" ))
    (setq brnd1 (getreal "\nRound to how many seconds?:"))
    (setq maxlen2 (getreal (strcat "\nFrom " (rtos maxlen1 2 3) " to:")))
    (setq brnd2 (getreal "\nRound to how many seconds?:"))
    (setq brnd3 (getreal (strcat "\nGreater than " (rtos maxlen2 2 3) " round to how many seconds?:")))
    (princ "\nDistances")
    (setq distmax1 (getreal "\nLess than:"))
    (setq drnd1 (getreal "\nRound to (in metres):") )
    (setq distmax2 (getreal (strcat "\nFrom " (rtos distmax1 2 3) " to:")))
    (setq drnd2  (getreal "\nRound to (in metres):") )
    (setq drnd3  (getreal (strcat "\nGreater than " (rtos distmax2 2 3) " round to (in metres):")) )

    ;store values in dwg
 (VLAX-LDATA-PUT "LXML4AC" "MAXLEN1" MAXLEN1)   
  (VLAX-LDATA-PUT "LXML4AC" "maxlen2" maxlen2)   
  (VLAX-LDATA-PUT "LXML4AC" "brnd1" brnd1)   
 (VLAX-LDATA-PUT "LXML4AC" "brnd2" brnd2)   
 (VLAX-LDATA-PUT "LXML4AC" "brnd3" brnd3)   
  (VLAX-LDATA-PUT "LXML4AC" "distmax1" distmax1)   
  (VLAX-LDATA-PUT "LXML4AC" "distmax2" distmax2)   
 (VLAX-LDATA-PUT "LXML4AC" "drnd1" drnd1)   
 (VLAX-LDATA-PUT "LXML4AC" "drnd2" drnd2)   
 (VLAX-LDATA-PUT "LXML4AC" "drnd3" drnd3)   
   
    )

;--------------------------Remove Element from List Function-----------------------------
(defun remove_nth ( lst n / lstn)
  (setq n (1+ n))
  (mapcar (function (lambda (x) (if (not (zerop (setq n (1- n))))(setq lstn (cons x lstn))))) lst)
  (reverse lstn)
  )
;---------------------------Replace element function-------------------------------------
(defun ReplaceItem (place item lst / lol i)

(if (and lst (>= (length lst) place))
(progn
(setq i 0)
(repeat (length lst)
(setq lol (cons (if (eq place (1+ i))
                        item
                          (nth i lst)
                        )
                        lol
                  )
        )
        (setq i (1+ i))
      );r
));p&if
 (reverse lol)
);defun
;--------------------------check polyline direction function------------------------------
 (defun LCW ( lst )
 
(apply '+ 
(mapcar
 (function
 (lambda ( a b )
 (- (* (car b) (cadr a)) (* (car a) (cadr b)))
 )
 ) lst (cons (last lst) lst)
 )
 )
 )
 
;---------------------------calcualate lot centre-----------------------------------------
(defun CALCLOTC (ptlist)
 ;calc lot centre if not specified
       
			 (setq easttot 0)
			 (setq northtot 0)
			 (if (= (nth 0 ptlist)(nth (- (length ptlist) 1) ptlist))(setq n (- (length ptlist) 1))(setq n (length ptlist)));make repeat for closed or unclosed list

			 (setq avgcnt 0)
			 (repeat n
			   (setq pnt (nth avgcnt ptlist))
       (setq east (nth 0 pnt))
    (setq north  (nth 1 pnt))
			   (setq easttot (+ easttot east))
			   (setq northtot (+ northtot north))
			   (setq avgcnt (+ avgcnt 1))
			   )
			 (setq lotc (list (/ easttot n)(/ northtot n)))
  )


;-------------------------------------------------------------------TRAVERSE-------------------------------------
(defun C:XTR (/)

  ;GET 1ST LINE INFO
    (graphscr)
    
    (setq p1 (getpoint "\nEnter start coords: "))
    (setq bearing (getstring "\nBearing(DD.MMSS): "))
        (setq dist (getstring T (strcat "\nDistance[Meters/Feet/DecimalFeet/Links]" units ":")))

  (if (or (= dist "f") (= dist "F") (= dist "Feet") (= dist "feet") (= dist "FEET"))
    (progn
      (setq dist (getstring T "\nDistance(Feet FF.II.n/d ): "))
      (setq units "F")
      )
    )
  
   (if (or (= dist "d")(= dist "df")(= dist "DF") (= dist "D") (= dist "DecimalFeet") (= dist "decimalfeet") (= DIST "DECIMALFEET"))
    (progn
      (setq dist (getstring T "\nDistance(Decimal Feet): "))
      (setq units "DF")
      )
    )

  (if (or (= dist "l") (= dist "L") (= dist "Links") (= dist "LINKS") (= DIST "links"))
    (progn
      (setq dist (getstring T "\nDistance(Links): "))
      (setq units "L")
      )
    )

    (if (or (= dist "m") (= dist "M") (= dist "Meters") (= dist "meters") (= DIST "METERS"))
    (progn
      (setq dist (getstring T "\nDistance(Meters): "))
      (setq units "M")
      )
    )
  (setq prevdist dist)
 

  (if (or (= units "F")(= units "L")(= units "DF"));label ye old distances when typeing them in for checking
    (setq labelyeolddist 1)
    (setq labelyeolddist 0)
    )
 ;APPLY ALL CORRECTIONS AND EXTRACT INFORMATION FROM USER INPUT

  ;reverse
  (if (or (= (substr bearing 1 1 ) "r") (= (substr bearing 1 1 ) "R" ))
    (progn
      (setq bearing (substr bearing 2 50))
      (setq rswitch "T")
      )
     (setq rswitch "F")
    )

;look for cardinals
  (setq card1 ""
	card2 "")
  (IF (OR (= (substr bearing 1 1 ) "n") (= (substr bearing 1 1 ) "N" )
	  (= (substr bearing 1 1 ) "s" )(= (substr bearing 1 1 ) "S" )
	  (= (substr bearing 1 1 ) "e" )(= (substr bearing 1 1 ) "E" )
	  (= (substr bearing 1 1 ) "w" )(= (substr bearing 1 1 ) "W" ))
(progn
    (setq card1 (substr bearing 1 1))
    (setq card2 (substr bearing (strlen bearing) 1))
    (setq bearing (substr bearing 2 (- (strlen bearing )2)))
  )
    )
    
  
(if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
  (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins  (strcat (substr bearing (+ dotpt1 2) 2) (chr 39)))
  (setq sec  (substr bearing (+ dotpt1 4) 10))

  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))

    (if (or
	(= (strlen sec) 2)
	(= (strlen mins) 2)
	(> (atof mins) 60)
	(> (atof sec) 60)
	(> (atof deg) 360)
	)
    (alert (strcat "That bearing looks a little funky - " bearing)))
  
  
  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

 ;correct cardinals

  (IF  (and (or (= card1 "n")(= card1 "N")(= card1 "s")(= card1 "S"))(and (/= card2 "w")(/= card2 "W")(/= card2 "e")(/= card2 "E")))
	  (alert (strcat "Cardinal missing E or W"))
    )
	  
  
  (if (and (or (= card1 "n")(= card1 "N"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (- 360 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
      
      
		 
      )
    );NW

		   

		   (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (- 270 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );WS

		   
		   (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (- 90 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );EN

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "e")(= card2 "E")))
    (progn
      (setq deg (rtos (- 180 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );SE

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (+ 180 (atof deg)) 2 0))
      
      )
    );SW

		    (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (+ 270 (atof deg)) 2 0))
      
      )
    );WN
		    (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (+ 90 (atof deg)) 2 0))
      
      )
    );ES
  
		   (if (and (= mins "")(= sec ""))(setq decimal "")(setq decimal "."))
	(setq lbearing (strcat deg decimal (substr mins 1 2) (substr sec 1 2)))
 (IF (= rswitch "T")(setq lbearing (strcat "R" lbearing)))
  
  



  ;look for line comment
  (setq spcpos1 (vl-string-position 32 dist 0))
	(if (= spcpos1 nil)(setq comment "")(progn
					      (setq comment (substr dist ( + spcpos1 2) ))
					      (setq dist (substr dist 1  spcpos1 ))
					      )
	  )
  


  
    (if (= units "F")
      (progn
	 (setq dotpos1 (vl-string-position 46 dist 0)) 
		    
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 dist (+ dotpos1 1))))
	(setq /pos1 (vl-string-position 47 dist 0))
	(if (/= /pos1 nil)
	  (progn
	    (setq den (substr dist ( + /pos1 2) 50))
	    (setq num (substr dist ( + dotpos2 2) (- (- /pos1 dotpos2) 1)))
	    (setq idist (/ (atof num) (atof den)))
	    (setq inches (substr dist (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq idist (+ idist (atof inches)))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (/= dotpos1 nil) (= /pos1 nil))
	  (progn
	    (setq inches (substr dist ( + dotpos1 2) 50))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist  (atof inches))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (= dotpos1 nil) (= /pos1 nil) (= dotpos2 nil))
	  (progn
	   
	    (setq feet (substr dist 1  50))
	    (setq idist (* (atof feet) 12))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
      )
    )
  (if (= units "L")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.201168)))
      )
    )
  (if (= units "DF")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.3048)))
      )
    )
	      
	      

    ;DRAW LINE 1
     
  (IF ( = rswitch "T")(setq obearing (substr lbearing 2 200))(setq obearing lbearing))
  (setq dist (rtos (atof dist)2 3));remove trailing zeros
  (if (= dist "0") (progn
		     (princ "\nDistance of 0 entered")
		     (exit))
    )
  
  (setq ldist dist)
  (setq bearing (strcat  deg "d" mins sec))

  (setq linetext (strcat "@" dist "<" bearing))
    (command "line" p1 linetext "")

;Add observation data to line as XDATA


  ;LOOK FOR SPECIAL COMMENTS
  (setq distancetype "" azimuthtype "")
  (IF (OR (= (STRCASE comment) "AD")(= (STRCASE comment) "ADOPT"))(setq distancetype " distanceType=\"Adopt Dimension\" " azimuthtype " azimuthType=\"Adopt Dimension\" " comment ""))
  (if (= (strcase (substr comment 1 3)) "AD ")(setq distancetype " distanceType=\"Adopt Dimension\" " azimuthtype " azimuthType=\"Adopt Dimension\" " ))
  (IF (OR (= (STRCASE comment) "COMP")(= (STRCASE comment) "COMPUTED"))(setq distancetype " distanceType=\"Computed\" " azimuthtype " azimuthType=\"Computed\" "  comment ""))
  (if (= (strcase (substr comment 1 5)) "COMP ")(setq distancetype " distanceType=\"Computed\" " azimuthtype " azimuthType=\"Computed\" " ))
  (IF (OR (= (STRCASE comment) "D")(= (STRCASE comment) "DERIVED"))(setq distancetype " distanceType=\"Derived\" " azimuthtype " azimuthType=\"Derived\" "  comment ""))
  (if (= (strcase (substr comment 1 8)) "DERIVED ")(setq distancetype " distanceType=\"Derived\" " azimuthtype " azimuthType=\"Derived\" " ))
  (IF (OR (= (STRCASE comment) "MEAS")(= (STRCASE comment) "MEASURED"))(setq distancetype " distanceType=\"Measured\" " azimuthtype " azimuthType=\"Measured\" "  comment ""))
  (if (= (strcase (substr comment 1 9)) "MEASURED ")(setq distancetype " distanceType=\"Measured\" " azimuthtype " azimuthType=\"Measured\" " ))
  
  (if (/= comment "")(setq ocomment (strcat "><FieldNote>\"" comment "\"</FieldNote></ReducedObservation>"))(setq ocomment "/>"))
  
(SETQ BDINFO (STRCAT "azimuth=\"" obearing "\" horizDistance=\"" ldist "\"" azimuthtype distancetype  ocomment))
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

  
;Move line if reverse activated
(if (= rswitch "T")
  (progn
    (setq p1 (CDR(ASSOC 10 sentlist)))
    (setq p2 (CDR(ASSOC 11 sentlist)))
    (command "move" sent "" p2 p1)

;get last line end point
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 10 sentlist)))    
    
    );p
  
;get last line end point if not reverse
  (progn
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 11 sentlist)))
);p else

  

);if
  (setq p1 (trans p1 0 1))

  (lba)
(setvar "clayer" prevlayer)

  ;GET 2ND+ LINE INFO
  (while (> 1 0) (progn
		   (setq bearing (getstring "\nBearing [Last]: "))

        (setq dist (getstring T (strcat "\nDistance [Last]" units ":")))

		   (if (or (= bearing "Last") (= bearing "L") (= bearing "l") (= bearing "" )) (setq bearing lbearing))
		   (if (or (= dist "Last") (= dist "L") (= dist "l") (= dist "")) (setq dist prevdist))
		   (setq prevdist dist)


 ;APPLY ALL CORRECTIONS AND EXTRACT INFORMATION FROM USER INPUT


  (if (or (= units "F")(= units "L")(= units "DF"));label ye old distances when typeing them in for checking
    (setq labelyeolddist 1)
    (setq labelyeolddist 0)
    )

  ;reverse
  (if (or (= (substr bearing 1 1 ) "r") (= (substr bearing 1 1 ) "R" ))
    (progn
      (setq bearing (substr bearing 2 50))
      (setq rswitch "T")
      )
     (setq rswitch "F")
    )

  ;look for cardinals
  (setq card1 ""
	card2 "")
  (IF (OR (= (substr bearing 1 1 ) "n") (= (substr bearing 1 1 ) "N" )
	  (= (substr bearing 1 1 ) "s" )(= (substr bearing 1 1 ) "S" )
	  (= (substr bearing 1 1 ) "e" )(= (substr bearing 1 1 ) "E" )
	  (= (substr bearing 1 1 ) "w" )(= (substr bearing 1 1 ) "W" ))
(progn
    (setq card1 (substr bearing 1 1))
    (setq card2 (substr bearing (strlen bearing) 1))
    (setq bearing (substr bearing 2 (- (strlen bearing )2)))
  )
    )
    
  
(if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
  (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins  (strcat (substr bearing (+ dotpt1 2) 2) (chr 39)))
  (setq sec  (substr bearing (+ dotpt1 4) 10))

  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))

    (if (or
	(= (strlen sec) 2)
	(= (strlen mins) 2)
	(> (atof mins) 60)
	(> (atof sec) 60)
	(> (atof deg) 360)
	)
    (alert (strcat "That bearing looks a little funky - " bearing)))
  
  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

 ;correct cardinals
		   
		     (IF  (and (or (= card1 "n")(= card1 "N")(= card1 "s")(= card1 "S"))(and (/= card2 "w")(/= card2 "W")(/= card2 "e")(/= card2 "E")))
	  (alert (strcat "Cardinal missing E or W"))
    )
		   
  (if (and (or (= card1 "n")(= card1 "N"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (- 360 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );NW

		   

		   (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (- 270 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );WS

		   
		   (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (- 90 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );EN

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "e")(= card2 "E")))
    (progn
      (setq deg (rtos (- 180 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );SE

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (+ 180 (atof deg)) 2 0))
      
      )
    );SW

		    (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (+ 270 (atof deg)) 2 0))
      
      )
    );WN
		    (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (+ 90 (atof deg)) 2 0))
      
      )
    );ES

		   (if (and (= mins "")(= sec ""))(setq decimal "")(setq decimal "."))
	(setq lbearing (strcat deg decimal (substr mins 1 2) (substr sec 1 2)))
 (IF (= rswitch "T")(setq lbearing (strcat "R" lbearing)))	   

  

  
		  
		   (setq bearing (strcat  deg "d" mins sec))

		   ;look for line comment
  (setq spcpos1 (vl-string-position 32 dist 0))
	(if (= spcpos1 nil)(setq comment "")(progn
					      (setq comment (substr dist ( + spcpos1 2) 50))
					      (setq dist (substr dist 1  spcpos1 ))
					      )
	  )
		   
		   (if (= units "F")
      (progn
(setq dotpos1 (vl-string-position 46 dist 0))
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 dist (+ dotpos1 1))))
	(setq /pos1 (vl-string-position 47 dist 0))
	(if (/= /pos1 nil)
	  (progn
	    (setq den (substr dist ( + /pos1 2) 50))
	    (setq num (substr dist ( + dotpos2 2) (- (- /pos1 dotpos2) 1)))
	    (setq idist (/ (atof num) (atof den)))
	    (setq inches (substr dist (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq idist (+ idist (atof inches)))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    );P
	  );IF
	(if (and (/= dotpos1 nil) (= /pos1 nil))
	  (progn
	    (setq inches (substr dist ( + dotpos1 2) 50))
	    (setq feet (substr dist 1   dotpos1 ))
	    (setq idist  (atof inches))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    );P
	  );IF
	(if (and (= dotpos1 nil) (= /pos1 nil) (= dotpos2 nil))
	  (progn
	   
	    (setq feet (substr dist 1  50))
	    (setq idist (* (atof feet) 12))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    );P
	  );IF
      );P
    );IF
  (if (= units "L")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.201168)))
      );P
    );IF
  (if (= units "DF")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.3048)))
          
      )); if & p decimal feet

		;DRAW LINE 2+

		    (setq dist (rtos (atof dist)2 3));remove trailing zeros
		   (if (= dist "0") (progn
		     (princ "\nDistance of 0 entered")
		     (exit))
    )
  (setq ldist dist)
	(IF ( = rswitch "T")(setq obearing (substr lbearing 2 200))(setq obearing lbearing))	   
    

  (setq linetext (strcat "@" dist "<" bearing))

(setq hardlink (rtos (getvar "CDATE") 2 8))
		   
  (command "line" p1 linetext "")

		     ;LOOK FOR SPECIAL COMMENTS
  (setq distancetype "" azimuthtype "")
  (IF (OR (= (STRCASE comment) "AD")(= (STRCASE comment) "ADOPT"))(setq distancetype " distanceType=\"Adopt Dimension\" " azimuthtype " azimuthType=\"Adopt Dimension\" " comment ""))
  (if (= (strcase (substr comment 1 3)) "AD ")(setq distancetype " distanceType=\"Adopt Dimension\" " azimuthtype " azimuthType=\"Adopt Dimension\" " ))
  (IF (OR (= (STRCASE comment) "COMP")(= (STRCASE comment) "COMPUTED"))(setq distancetype " distanceType=\"Computed\" " azimuthtype " azimuthType=\"Computed\" "  comment ""))
  (if (= (strcase (substr comment 1 5)) "COMP ")(setq distancetype " distanceType=\"Computed\" " azimuthtype " azimuthType=\"Computed\" " ))
  (IF (OR (= (STRCASE comment) "D")(= (STRCASE comment) "DERIVED"))(setq distancetype " distanceType=\"Derived\" " azimuthtype " azimuthType=\"Derived\" "  comment ""))
  (if (= (strcase (substr comment 1 8)) "DERIVED ")(setq distancetype " distanceType=\"Derived\" " azimuthtype " azimuthType=\"Derived\" " ))
  (IF (OR (= (STRCASE comment) "MEAS")(= (STRCASE comment) "MEASURED"))(setq distancetype " distanceType=\"Measured\" " azimuthtype " azimuthType=\"Measured\" "  comment ""))
  (if (= (strcase (substr comment 1 9)) "MEASURED ")(setq distancetype " distanceType=\"Measured\" " azimuthtype " azimuthType=\"Measured\" " ))
		   
;Add observation data to line as XDATA
(if (/= comment "")(setq ocomment (strcat "><FieldNote>\"" comment "\"</FieldNote></ReducedObservation>"))(setq ocomment "/>"))
(SETQ BDINFO (STRCAT "azimuth=\"" obearing "\" horizDistance=\"" ldist "\"" azimuthtype distancetype  ocomment))
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
 ;Move line if reverse activated
(if (= rswitch "T")
  (progn
    (setq p1 (CDR(ASSOC 10 sentlist)))
    (setq p2 (CDR(ASSOC 11 sentlist)))
    (command "move" sent "" p2 p1)

;get last line end point
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 10 sentlist)))    
    
    );p
  
;get last line end point if not reverse
  (progn
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 11 sentlist)))
);p else

  
  
);if

		   (setq p1 (trans p1 0 1))

(lba)		   
(setvar "clayer" prevlayer)
		   
  
);P
    );WHILE 1>0
  );DEFUN






;-------------------------------------------------------------------SOUTH AUSTRAILIAN ANGLE TRAVERSE-------------------------------------
(defun C:XTS (/)

  ;GET 1ST LINE INFO
    (graphscr)
    
    (setq STARTOBJS (ENTSEL "\nSelect Starting Line: "))
  (setq startobj (car startobjs))
  (setq closept (cadr startobjs))

  (setq lp1 (CDR(ASSOC 10 (entget startobj))))
  (setq lp2 (cdr(assoc 11 (entget startobj))))

  (if (< (distance lp1 closept)(distance lp2 closept))
    (setq ang (angle lp1 lp2)
	  p1 lp1)
    (setq ang (angle lp2 lp1)
	  p1 lp2)
    );if select was closer to 1
  

      (SETQ BEARING (ANGTOS ANG 1 4));REQUIRED FOR ELSE ROUND
  (setq bearing (vl-string-subst "d" (chr 176) bearing));added for BricsCAD changes degrees to "d"

 (SETQ DPOS (vl-string-position 100 BEARING 0))
  (setq Wpos (vl-string-position 39 BEARING 0))
  (setq WWpos (vl-string-position 34 BEARING 0))

    (setq sDEG (substr BEARING 1 Dpos))
      (setq sMINS (substr BEARING (+ Dpos 2) (- (- WPOS DPOS) 1)))
      (setq sSEC (substr BEARING (+ Wpos 2) (- (- WWpos Wpos) 1)))

  


  
    (setq bearing (getstring "\nAngle(DD.MMSS)(negative for anticlokwise): "))
  (setq langle bearing)
   (if (or (= (substr bearing 1 1) "D")(= (substr bearing 1 1) "d"))(setq deflect 180
									  bearing (substr bearing 2))
		     (setq deflect 0)
		     )
  (if (= (substr bearing 1 1) "-")(setq -switch -1
					bearing (substr bearing 2))
    (setq -switch 1)
    )
        (setq dist (getstring T (strcat "\nDistance[Meters/Feet/DecimalFeet/Links]" units ":")))

  (if (or (= dist "f") (= dist "F") (= dist "Feet") (= dist "feet") (= dist "FEET"))
    (progn
      (setq dist (getstring T "\nDistance(Feet FF.II.n/d ): "))
      (setq units "F")
      )
    )
  
   (if (or (= dist "d")(= dist "df")(= dist "DF") (= dist "D") (= dist "DecimalFeet") (= dist "decimalfeet") (= DIST "DECIMALFEET"))
    (progn
      (setq dist (getstring T "\nDistance(Decimal Feet): "))
      (setq units "DF")
      )
    )

  (if (or (= dist "l") (= dist "L") (= dist "Links") (= dist "LINKS") (= DIST "links"))
    (progn
      (setq dist (getstring T "\nDistance(Links): "))
      (setq units "L")
      )
    )

    (if (or (= dist "m") (= dist "M") (= dist "Meters") (= dist "meters") (= DIST "METERS"))
    (progn
      (setq dist (getstring T "\nDistance(Meters): "))
      (setq units "M")
      )
    )
  (setq prevdist dist)
 

  (if (or (= units "F")(= units "L")(= units "DF"));label ye old distances when typeing them in for checking
    (setq labelyeolddist 1)
    (setq labelyeolddist 0)
    )
 ;APPLY ALL CORRECTIONS AND EXTRACT INFORMATION FROM USER INPUT

  ;reverse
  (if (or (= (substr bearing 1 1 ) "r") (= (substr bearing 1 1 ) "R" ))
    (progn
      (setq bearing (substr bearing 2 50))
      (setq rswitch "T")
      )
     (setq rswitch "F")
    )

;look for cardinals
  (setq card1 ""
	card2 "")
  (IF (OR (= (substr bearing 1 1 ) "n") (= (substr bearing 1 1 ) "N" )
	  (= (substr bearing 1 1 ) "s" )(= (substr bearing 1 1 ) "S" )
	  (= (substr bearing 1 1 ) "e" )(= (substr bearing 1 1 ) "E" )
	  (= (substr bearing 1 1 ) "w" )(= (substr bearing 1 1 ) "W" ))
(progn
    (setq card1 (substr bearing 1 1))
    (setq card2 (substr bearing (strlen bearing) 1))
    (setq bearing (substr bearing 2 (- (strlen bearing )2)))
  )
    )
    
  
(if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
  (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins  (strcat (substr bearing (+ dotpt1 2) 2) (chr 39)))
  (setq sec  (substr bearing (+ dotpt1 4) 10))

  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))

    (if (or
	(= (strlen sec) 2)
	(= (strlen mins) 2)
	(> (atof mins) 60)
	(> (atof sec) 60)
	(> (atof deg) 360)
	)
    (alert (strcat "That bearing looks a little funky - " bearing)))
  
  
  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

 ;correct cardinals

  (IF  (and (or (= card1 "n")(= card1 "N")(= card1 "s")(= card1 "S"))(and (/= card2 "w")(/= card2 "W")(/= card2 "e")(/= card2 "E")))
	  (alert (strcat "Cardinal missing E or W"))
    )
	  
  
  (if (and (or (= card1 "n")(= card1 "N"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (- 360 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
      
      
		 
      )
    );NW

		   

		   (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (- 270 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );WS

		   
		   (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (- 90 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );EN

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "e")(= card2 "E")))
    (progn
      (setq deg (rtos (- 180 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );SE

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (+ 180 (atof deg)) 2 0))
      
      )
    );SW

		    (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (+ 270 (atof deg)) 2 0))
      
      )
    );WN
		    (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (+ 90 (atof deg)) 2 0))
      
      )
    );ES
  
		   (if (and (= mins "")(= sec ""))(setq decimal "")(setq decimal "."))
	(setq lbearing (strcat deg decimal (substr mins 1 2) (substr sec 1 2)))
 (IF (= rswitch "T")(setq lbearing (strcat "R" lbearing)))
  
  



  ;look for line comment
  (setq spcpos1 (vl-string-position 32 dist 0))
	(if (= spcpos1 nil)(setq comment "")(progn
					      (setq comment (substr dist ( + spcpos1 2) ))
					      (setq dist (substr dist 1  spcpos1 ))
					      )
	  )
  


  
    (if (= units "F")
      (progn
	 (setq dotpos1 (vl-string-position 46 dist 0)) 
		    
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 dist (+ dotpos1 1))))
	(setq /pos1 (vl-string-position 47 dist 0))
	(if (/= /pos1 nil)
	  (progn
	    (setq den (substr dist ( + /pos1 2) 50))
	    (setq num (substr dist ( + dotpos2 2) (- (- /pos1 dotpos2) 1)))
	    (setq idist (/ (atof num) (atof den)))
	    (setq inches (substr dist (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq idist (+ idist (atof inches)))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (/= dotpos1 nil) (= /pos1 nil))
	  (progn
	    (setq inches (substr dist ( + dotpos1 2) 50))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist  (atof inches))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (= dotpos1 nil) (= /pos1 nil) (= dotpos2 nil))
	  (progn
	   
	    (setq feet (substr dist 1  50))
	    (setq idist (* (atof feet) 12))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
      )
    )
  (if (= units "L")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.201168)))
      )
    )
  (if (= units "DF")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.3048)))
      )
    )

  
    (setq deg  (+ deflect (atof sdeg)(* -switch (atof deg))))
  (setq mins  (+ (atof smins)(* -switch (atof mins))))
  (setq sec  (+ (atof ssec)(* -switch (atof sec))))

  (IF (>= sec 60) (setq mins (+ mins 1)
			sec (- sec 60)))
  (if (< sec 0)(setq mins (- mins 1)
		      sec (+ sec 60)))
  (if (>= mins 60) (setq deg (+ deg 1)
			 mins (- mins 60)))
  (if (< mins 0)(setq deg (- deg 1)
		       mins (+ mins 60)))
  (if (>= deg 360)(setq deg (- deg 360)))
  (if (< deg 0)(setq deg (+ deg 360)))

  (setq deg (rtos deg 2 0))
  (setq mins (rtos mins 2 0))
  (setq sec (rtos sec 2))

  	   (if (= (strlen mins) 1)(setq mins (strcat "0" mins)))
		   (if (= (strlen sec) 1)(setq sec(strcat "0" sec)))		   

		   (setq bearing (strcat  deg "d" mins (chr 39) sec (chr 34)))

  (if (and (= mins "00")(= sec "00"))(setq mins ""
					 sec ""
					   bearing (strcat deg "d")))
		   (if (and (= sec "00")(/= mins "00"))(setq sec ""
							     bearing (strcat deg "d" mins (chr 39))))
 
		     
   (if (and (= mins "")(= sec ""))(setq decimal "")(setq decimal "."))
	(setq lbearing (strcat deg decimal (substr mins 1 2) (substr sec 1 2)))
  
      
	      
	      

    ;DRAW LINE 1
     
  (IF ( = rswitch "T")(setq obearing (substr lbearing 2 200))(setq obearing lbearing))
  (setq dist (rtos (atof dist)2 3));remove trailing zeros
  (if (= dist "0") (progn
		     (princ "\nDistance of 0 entered")
		     (exit))
    )
  
  (setq ldist dist)
  

  (setq linetext (strcat "@" dist "<" bearing))
    (command "line" p1 linetext "")

;Add observation data to line as XDATA


  ;LOOK FOR SPECIAL COMMENTS
  (setq distancetype "" azimuthtype "")
  (IF (OR (= (STRCASE comment) "AD")(= (STRCASE comment) "ADOPT"))(setq distancetype " distanceType=\"Adopt Dimension\" " azimuthtype " azimuthType=\"Adopt Dimension\" " comment ""))
  (if (= (strcase (substr comment 1 3)) "AD ")(setq distancetype " distanceType=\"Adopt Dimension\" " azimuthtype " azimuthType=\"Adopt Dimension\" " ))
  (IF (OR (= (STRCASE comment) "COMP")(= (STRCASE comment) "COMPUTED"))(setq distancetype " distanceType=\"Computed\" " azimuthtype " azimuthType=\"Computed\" "  comment ""))
  (if (= (strcase (substr comment 1 5)) "COMP ")(setq distancetype " distanceType=\"Computed\" " azimuthtype " azimuthType=\"Computed\" " ))
  (IF (OR (= (STRCASE comment) "D")(= (STRCASE comment) "DERIVED"))(setq distancetype " distanceType=\"Derived\" " azimuthtype " azimuthType=\"Derived\" "  comment ""))
  (if (= (strcase (substr comment 1 8)) "DERIVED ")(setq distancetype " distanceType=\"Derived\" " azimuthtype " azimuthType=\"Derived\" " ))
  (IF (OR (= (STRCASE comment) "MEAS")(= (STRCASE comment) "MEASURED"))(setq distancetype " distanceType=\"Measured\" " azimuthtype " azimuthType=\"Measured\" "  comment ""))
  (if (= (strcase (substr comment 1 9)) "MEASURED ")(setq distancetype " distanceType=\"Measured\" " azimuthtype " azimuthType=\"Measured\" " ))
  
  (if (/= comment "")(setq ocomment (strcat "><FieldNote>\"" comment "\"</FieldNote></ReducedObservation>"))(setq ocomment "/>"))
  
(SETQ BDINFO (STRCAT "azimuth=\"" obearing "\" horizDistance=\"" ldist "\"" azimuthtype distancetype  ocomment))
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

  
;Move line if reverse activated
(if (= rswitch "T")
  (progn
    (setq p1 (CDR(ASSOC 10 sentlist)))
    (setq p2 (CDR(ASSOC 11 sentlist)))
    (command "move" sent "" p2 p1)

;get last line end point
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 10 sentlist)))
    (setq lp1 (CDR(ASSOC 10 sentlist)))
    (setq lp2 (CDR(ASSOC 11 sentlist)))
    
    );p
  
;get last line end point if not reverse
  (progn
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 11 sentlist)))
    (setq lp1 (CDR(ASSOC 11 sentlist)))
    (setq lp2 (CDR(ASSOC 10 sentlist)))
);p else

);if


  (lba)
  
  ;setup new previous line
  (setq ang (angle lp1 lp2))
  
    (SETQ BEARING (ANGTOS ANG 1 4));REQUIRED FOR ELSE ROUND
  (setq bearing (vl-string-subst "d" (chr 176) bearing));added for BricsCAD changes degrees to "d"

 (SETQ DPOS (vl-string-position 100 BEARING 0))
  (setq Wpos (vl-string-position 39 BEARING 0))
  (setq WWpos (vl-string-position 34 BEARING 0))

    (setq sDEG (substr BEARING 1 Dpos))
      (setq sMINS (substr BEARING (+ Dpos 2) (- (- WPOS DPOS) 1)))
      (setq sSEC (substr BEARING (+ Wpos 2) (- (- WWpos Wpos) 1)))
  


  (setq p1 (trans p1 0 1))

  
(setvar "clayer" prevlayer)

  

  ;GET 2ND+ LINE INFO*********************************************************************************************************************************************************
  (while (> 1 0) (progn
		   (setq bearing (getstring "\nAngle[Last]: "))
		   (if (or (= bearing "Last") (= bearing "L") (= bearing "l") (= bearing "" )) (setq bearing langle))
		   (setq langle bearing)
		   (if (or (= (substr bearing 1 1) "D")(= (substr bearing 1 1) "d"))(setq deflect 180
											  bearing (substr bearing 2))
		     (setq deflect 0)
		     )
		     (if (= (substr bearing 1 1) "-")(setq -switch -1
					bearing (substr bearing 2))
    (setq -switch 1)
    )

        (setq dist (getstring T (strcat "\nDistance [Last]" units ":")))


		   (if (or (= dist "Last") (= dist "L") (= dist "l") (= dist "")) (setq dist prevdist))
		   (setq prevdist dist)


 ;APPLY ALL CORRECTIONS AND EXTRACT INFORMATION FROM USER INPUT


  (if (or (= units "F")(= units "L")(= units "DF"));label ye old distances when typeing them in for checking
    (setq labelyeolddist 1)
    (setq labelyeolddist 0)
    )

  ;reverse
  (if (or (= (substr bearing 1 1 ) "r") (= (substr bearing 1 1 ) "R" ))
    (progn
      (setq bearing (substr bearing 2 50))
      (setq rswitch "T")
      )
     (setq rswitch "F")
    )

  ;look for cardinals
  (setq card1 ""
	card2 "")
  (IF (OR (= (substr bearing 1 1 ) "n") (= (substr bearing 1 1 ) "N" )
	  (= (substr bearing 1 1 ) "s" )(= (substr bearing 1 1 ) "S" )
	  (= (substr bearing 1 1 ) "e" )(= (substr bearing 1 1 ) "E" )
	  (= (substr bearing 1 1 ) "w" )(= (substr bearing 1 1 ) "W" ))
(progn
    (setq card1 (substr bearing 1 1))
    (setq card2 (substr bearing (strlen bearing) 1))
    (setq bearing (substr bearing 2 (- (strlen bearing )2)))
  )
    )
    
  
(if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
  (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins  (strcat (substr bearing (+ dotpt1 2) 2) (chr 39)))
  (setq sec  (substr bearing (+ dotpt1 4) 10))

  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))

    (if (or
	(= (strlen sec) 2)
	(= (strlen mins) 2)
	(> (atof mins) 60)
	(> (atof sec) 60)
	(> (atof deg) 360)
	)
    (alert (strcat "That bearing looks a little funky - " bearing)))
  
  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

 ;correct cardinals
		   
		     (IF  (and (or (= card1 "n")(= card1 "N")(= card1 "s")(= card1 "S"))(and (/= card2 "w")(/= card2 "W")(/= card2 "e")(/= card2 "E")))
	  (alert (strcat "Cardinal missing E or W"))
    )
		   
  (if (and (or (= card1 "n")(= card1 "N"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (- 360 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );NW

		   

		   (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (- 270 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );WS

		   
		   (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (- 90 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );EN

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "e")(= card2 "E")))
    (progn
      (setq deg (rtos (- 180 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );SE

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (+ 180 (atof deg)) 2 0))
      
      )
    );SW

		    (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (+ 270 (atof deg)) 2 0))
      
      )
    );WN
		    (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (+ 90 (atof deg)) 2 0))
      
      )
    );ES

		   (if (and (= mins "")(= sec ""))(setq decimal "")(setq decimal "."))
	(setq lbearing (strcat deg decimal (substr mins 1 2) (substr sec 1 2)))
 (IF (= rswitch "T")(setq lbearing (strcat "R" lbearing)))	   

  

  
		  
		   (setq bearing (strcat  deg "d" mins sec))

		   ;look for line comment
  (setq spcpos1 (vl-string-position 32 dist 0))
	(if (= spcpos1 nil)(setq comment "")(progn
					      (setq comment (substr dist ( + spcpos1 2) 50))
					      (setq dist (substr dist 1  spcpos1 ))
					      )
	  )
		   
		   (if (= units "F")
      (progn
(setq dotpos1 (vl-string-position 46 dist 0))
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 dist (+ dotpos1 1))))
	(setq /pos1 (vl-string-position 47 dist 0))
	(if (/= /pos1 nil)
	  (progn
	    (setq den (substr dist ( + /pos1 2) 50))
	    (setq num (substr dist ( + dotpos2 2) (- (- /pos1 dotpos2) 1)))
	    (setq idist (/ (atof num) (atof den)))
	    (setq inches (substr dist (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq idist (+ idist (atof inches)))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    );P
	  );IF
	(if (and (/= dotpos1 nil) (= /pos1 nil))
	  (progn
	    (setq inches (substr dist ( + dotpos1 2) 50))
	    (setq feet (substr dist 1   dotpos1 ))
	    (setq idist  (atof inches))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    );P
	  );IF
	(if (and (= dotpos1 nil) (= /pos1 nil) (= dotpos2 nil))
	  (progn
	   
	    (setq feet (substr dist 1  50))
	    (setq idist (* (atof feet) 12))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    );P
	  );IF
      );P
    );IF
  (if (= units "L")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.201168)))
      );P
    );IF
  (if (= units "DF")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.3048)))
          
      )); if & p decimal feet





  (setq deg  (+ deflect (atof sdeg)(* -switch (atof deg))))
  (setq mins  (+ (atof smins)(* -switch (atof mins))))
  (setq sec  (+ (atof ssec)(* -switch (atof sec))))

  (IF (>= sec 60) (setq mins (+ mins 1)
			sec (- sec 60)))
  (if (< sec 0)(setq mins (- mins 1)
		      sec (+ sec 60)))
  (if (>= mins 60) (setq deg (+ deg 1)
			 mins (- mins 60)))
  (if (< mins 0)(setq deg (- deg 1)
		       mins (+ mins 60)))
  (if (>= deg 360)(setq deg (- deg 360)))
  (if (< deg 0)(setq deg (+ deg 360)))

  (setq deg (rtos deg 2 0))
  (setq mins (rtos mins 2 0))
  (setq sec (rtos sec 2))
		   
 	   (if (= (strlen mins) 1)(setq mins (strcat "0" mins)))
		   (if (= (strlen sec) 1)(setq sec(strcat "0" sec)))		   

		   (setq bearing (strcat  deg "d" mins (chr 39) sec (chr 34)))

  (if (and (= mins "00")(= sec "00"))(setq mins ""
					 sec ""
					   bearing (strcat deg "d")))
		   (if (and (= sec "00")(/= mins "00"))(setq sec ""
							     bearing (strcat deg "d" mins (chr 39))))
 
		     
   (if (and (= mins "")(= sec ""))(setq decimal "")(setq decimal "."))
	(setq lbearing (strcat deg decimal (substr mins 1 2) (substr sec 1 2)))
  

 


		   

		;DRAW LINE 2+

		    (setq dist (rtos (atof dist)2 3));remove trailing zeros
		   (if (= dist "0") (progn
		     (princ "\nDistance of 0 entered")
		     (exit))
    )
		  
		   
  (setq ldist dist)
	(IF ( = rswitch "T")(setq obearing (substr lbearing 2 200))(setq obearing lbearing))	   
    

  (setq linetext (strcat "@" dist "<" bearing))

(setq hardlink (rtos (getvar "CDATE") 2 8))
		   
  (command "line" p1 linetext "")

		     ;LOOK FOR SPECIAL COMMENTS
  (setq distancetype "" azimuthtype "")
  (IF (OR (= (STRCASE comment) "AD")(= (STRCASE comment) "ADOPT"))(setq distancetype " distanceType=\"Adopt Dimension\" " azimuthtype " azimuthType=\"Adopt Dimension\" " comment ""))
  (if (= (strcase (substr comment 1 3)) "AD ")(setq distancetype " distanceType=\"Adopt Dimension\" " azimuthtype " azimuthType=\"Adopt Dimension\" " ))
  (IF (OR (= (STRCASE comment) "COMP")(= (STRCASE comment) "COMPUTED"))(setq distancetype " distanceType=\"Computed\" " azimuthtype " azimuthType=\"Computed\" "  comment ""))
  (if (= (strcase (substr comment 1 5)) "COMP ")(setq distancetype " distanceType=\"Computed\" " azimuthtype " azimuthType=\"Computed\" " ))
  (IF (OR (= (STRCASE comment) "D")(= (STRCASE comment) "DERIVED"))(setq distancetype " distanceType=\"Derived\" " azimuthtype " azimuthType=\"Derived\" "  comment ""))
  (if (= (strcase (substr comment 1 8)) "DERIVED ")(setq distancetype " distanceType=\"Derived\" " azimuthtype " azimuthType=\"Derived\" " ))
  (IF (OR (= (STRCASE comment) "MEAS")(= (STRCASE comment) "MEASURED"))(setq distancetype " distanceType=\"Measured\" " azimuthtype " azimuthType=\"Measured\" "  comment ""))
  (if (= (strcase (substr comment 1 9)) "MEASURED ")(setq distancetype " distanceType=\"Measured\" " azimuthtype " azimuthType=\"Measured\" " ))
		   
;Add observation data to line as XDATA
(if (/= comment "")(setq ocomment (strcat "><FieldNote>\"" comment "\"</FieldNote></ReducedObservation>"))(setq ocomment "/>"))
(SETQ BDINFO (STRCAT "azimuth=\"" obearing "\" horizDistance=\"" ldist "\"" azimuthtype distancetype  ocomment))
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
 
;Move line if reverse activated
(if (= rswitch "T")
  (progn
    (setq p1 (CDR(ASSOC 10 sentlist)))
    (setq p2 (CDR(ASSOC 11 sentlist)))
    (command "move" sent "" p2 p1)

;get last line end point
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 10 sentlist)))
    (setq lp1 (CDR(ASSOC 10 sentlist)))
    (setq lp2 (CDR(ASSOC 11 sentlist)))
    
    );p
  
;get last line end point if not reverse
  (progn
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 11 sentlist)))
    (setq lp1 (CDR(ASSOC 11 sentlist)))
    (setq lp2 (CDR(ASSOC 10 sentlist))) 
);p else


 
  
);if

		   (lba)

		     ;setup new previous line
  (setq ang (angle lp1 lp2))
  
    (SETQ BEARING (ANGTOS ANG 1 4));REQUIRED FOR ELSE ROUND
  (setq bearing (vl-string-subst "d" (chr 176) bearing));added for BricsCAD changes degrees to "d"

 (SETQ DPOS (vl-string-position 100 BEARING 0))
  (setq Wpos (vl-string-position 39 BEARING 0))
  (setq WWpos (vl-string-position 34 BEARING 0))

    (setq sDEG (substr BEARING 1 Dpos))
      (setq sMINS (substr BEARING (+ Dpos 2) (- (- WPOS DPOS) 1)))
      (setq sSEC (substr BEARING (+ Wpos 2) (- (- WWpos Wpos) 1)))
		   

		   (setq p1 (trans p1 0 1))

		   
(setvar "clayer" prevlayer)
		   
  
);P
    );WHILE 1>0
  );DEFUN

;-------------------------------------------------------------------ARCS TRAVERSE-------------------------------------
(defun C:XTA (/)
  (setq prevlayer (getvar "CLAYER"))
  ;GET 1ST LINE INFO
    (graphscr)
    
    (setq p1 (getpoint "\nEnter start coords: "))
    (setq bearing (getstring "\nChord Bearing(DD.MMSS): "))
       
  (setq dist (getstring T (strcat "\nChord Distance[Meters/Feet/DecimalFeet/Links/ArcDistance]" units ":")))

  (if (or (= dist "f") (= dist "F") (= dist "Feet") (= dist "feet") (= dist "FEET"))
    (progn
      (setq dist (getstring T "\nDistance(Feet FF.II.n/d ): "))
      (setq units "F")
      )
    )
  
   (if (or (= dist "d")(= dist "df")(= dist "DF") (= dist "D") (= dist "DecimalFeet") (= dist "decimalfeet") (= DIST "DECIMALFEET"))
    (progn
      (setq dist (getstring T "\nDistance(Decimal Feet): "))
      (setq units "DF")
      )
    )

  (if (or (= dist "l") (= dist "L") (= dist "Links") (= dist "LINKS") (= DIST "links"))
    (progn
      (setq dist (getstring T "\nDistance(Links): "))
      (setq units "L")
      )
    )
   (if (or (= dist "m") (= dist "M") (= dist "Meters") (= dist "meters") (= DIST "METERS"))
    (progn
      (setq dist (getstring T "\nDistance(Meters): "))
      (setq units "M")
      )
    )

;ARC DISTANCE
  (if (or (= dist "a") (= dist "A") (= dist "ArcDistance") (= dist "arcdistance") (= DIST "ARCDISTANCE"))
    (progn

      (setq dist (getstring T (strcat "\nArc Distance[Meters/Feet/DecimalFeet/Links/ArcDistance]" units ":")))

  (if (or (= dist "f") (= dist "F") (= dist "Feet") (= dist "feet") (= dist "FEET"))
    (progn
      (setq dist (getstring T "\nDistance(Feet FF.II.n/d ): "))
      (setq units "F")
      )
    )
  
   (if (or (= dist "d")(= dist "df")(= dist "DF") (= dist "D") (= dist "DecimalFeet") (= dist "decimalfeet") (= DIST "DECIMALFEET"))
    (progn
      (setq dist (getstring T "\nDistance(Decimal Feet): "))
      (setq units "DF")
      )
    )

  (if (or (= dist "l") (= dist "L") (= dist "Links") (= dist "LINKS") (= DIST "links"))
    (progn
      (setq dist (getstring T "\nDistance(Links): "))
      (setq units "L")
      )
    )
   (if (or (= dist "m") (= dist "M") (= dist "Meters") (= dist "meters") (= DIST "METERS"))
    (progn
      (setq dist (getstring T "\nDistance(Meters): "))
      (setq units "M")
      )
    )

      

(setq arcdist "Y")


      
      );if arc
    ;else
    (setq arcdist "N")

    );ARC DISTANCE

  

  
  (setq prevdist dist)
  

 (setq radius (getstring (strcat "\nRadius (" units "):")))
(setq prevradius radius)

  ;look for line comment
  (setq spcpos1 (vl-string-position 32 dist 0))
	(if (= spcpos1 nil)(setq comment "")(progn
					      (setq comment (substr dist ( + spcpos1 2) 50))
					      (setq dist (substr dist 1  spcpos1 ))
					      )
	  )

  
 ;APPLY ALL CORRECTIONS AND EXTRACT INFORMATION FROM USER INPUT


  ;reverse
  (if (or (= (substr bearing 1 1 ) "r") (= (substr bearing 1 1 ) "R" ))
    (progn
      (setq bearing (substr bearing 2 50))
      (setq rswitch "T")
      )
     (setq rswitch "F")
    )

;look for cardinals
  (setq card1 ""
	card2 "")
  (IF (OR (= (substr bearing 1 1 ) "n") (= (substr bearing 1 1 ) "N" )
	  (= (substr bearing 1 1 ) "s" )(= (substr bearing 1 1 ) "S" )
	  (= (substr bearing 1 1 ) "e" )(= (substr bearing 1 1 ) "E" )
	  (= (substr bearing 1 1 ) "w" )(= (substr bearing 1 1 ) "W" ))
(progn
    (setq card1 (substr bearing 1 1))
    (setq card2 (substr bearing (strlen bearing) 1))
    (setq bearing (substr bearing 2 (- (strlen bearing )2)))
  )
    )
    
  
(if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
  (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins  (strcat (substr bearing (+ dotpt1 2) 2) (chr 39)))
  (setq sec  (substr bearing (+ dotpt1 4) 10))

  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))

    (if (or
	(= (strlen sec) 2)
	(= (strlen mins) 2)
	(> (atof mins) 60)
	(> (atof sec) 60)
	(> (atof deg) 360)
	)
    (alert (strcat "That bearing looks a little funky - " bearing)))
  
  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

 ;correct cardinals

    (IF  (and (or (= card1 "n")(= card1 "N")(= card1 "s")(= card1 "S"))(and (/= card2 "w")(/= card2 "W")(/= card2 "e")(/= card2 "E")))
	  (alert (strcat "Cardinal missing E or W"))
    )
  
  (if (and (or (= card1 "n")(= card1 "N"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (- 360 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );NW

		   

		   (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (- 270 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );WS

		   
		   (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (- 90 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );EN

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "e")(= card2 "E")))
    (progn
      (setq deg (rtos (- 180 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );SE

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (+ 180 (atof deg)) 2 0))
      
      )
    );SW

		    (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (+ 270 (atof deg)) 2 0))
      
      )
    );WN
		    (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (+ 90 (atof deg)) 2 0))
      
      )
    );ES
  
	
  		   (if (and (= mins "")(= sec ""))(setq decimal "")(setq decimal "."))
	(setq lbearing (strcat deg decimal (substr mins 1 2) (substr sec 1 2)))
 (IF (= rswitch "T")(setq lbearing (strcat "R" lbearing)))
    
     
  

  (setq bearing (strcat  deg "d" mins sec))
  
   
    (if (= units "F")
      (progn
	(setq dotpos1 (vl-string-position 46 dist 0))
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 dist (+ dotpos1 1))))
	(setq /pos1 (vl-string-position 47 dist 0))
	(if (/= /pos1 nil)
	  (progn
	    (setq den (substr dist ( + /pos1 2) 50))
	    (setq num (substr dist ( + dotpos2 2) (- (- /pos1 dotpos2) 1)))
	    (setq idist (/ (atof num) (atof den)))
	    (setq inches (substr dist (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq idist (+ idist (atof inches)))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (/= dotpos1 nil) (= /pos1 nil))
	  (progn
	    (setq inches (substr dist ( + dotpos1 2) 50))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist  (atof inches))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (= dotpos1 nil) (= /pos1 nil) (= dotpos2 nil))
	  (progn
	   
	    (setq feet (substr dist 1  50))
	    (setq idist (* (atof feet) 12))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
;radius in feet
	
	(setq dotpos1 (vl-string-position 46 radius 0))
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 radius (+ dotpos1 1))))
	(setq /pos1 (vl-string-position 47 radius 0))
	(if (/= /pos1 nil)
	  (progn
	    (setq den (substr radius ( + /pos1 2) 50))
	    (setq num (substr radius ( + dotpos2 2) (- (- /pos1 dotpos2) 1)))
	    (setq idist (/ (atof num) (atof den)))
	    (setq inches (substr radius (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq idist (+ idist (atof inches)))
	    (setq feet (substr radius 1  dotpos1 ))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq radius (rtos (* idist 0.0254)))
	    )
	  )
	(if (and (/= dotpos1 nil) (= /pos1 nil))
	  (progn
	    (setq inches (substr radius ( + dotpos1 2) 50))
	    (setq feet (substr radius 1  dotpos1 ))
	    (setq idist  (atof inches))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq radius (rtos (* idist 0.0254)))
	    )
	  )
	(if (and (= dotpos1 nil) (= /pos1 nil) (= dotpos2 nil))
	  (progn
	   
	    (setq feet (substr radius 1  50))
	    (setq idist (* (atof feet) 12))
	    (setq radius (rtos (* idist 0.0254)))
	    )
	  )
	

	
	));if & p feet

  
  (if (= units "L")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.201168)))
      (setq radius (atof radius))
      (setq radius (rtos (* radius 0.201168)))
      
      )); if & p links

   (if (= units "DF")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.3048)))
      (setq radius (atof radius))
      (setq radius (rtos (* radius 0.3048)))
      
      )); if & p decimal feet


  (if (= arcdist "Y")(setq dist (rtos (* (* 2 (atof radius)) (sin (/ (atof dist) (* 2 (atof radius))))))))
       
	      

    ;DRAW LINE 1
  
   (setq dist (rtos (atof dist)2 3));remove trailing zeros
  (if (= dist "0") (progn
		     (princ "\nDistance of 0 entered")
		     (exit))
    )
   
  (setq dist (rtos (atof dist)2 3));remove trailing zeros
  (setq ldist dist)
  (setq lradius radius)

  (setq linetext (strcat "@" dist "<" bearing))
 
  (command "line" p1 linetext "")
(setvar "CLAYER" prevlayer )
  



  
;Move line if reverse activated
(if (= rswitch "T")
  (progn
      (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
    (setq p1 (CDR(ASSOC 10 sentlist)))
    (setq p2 (CDR(ASSOC 11 sentlist)))
    (command "move" sent "" p2 p1)

;get last line end point
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 10 sentlist)))
    (setq ap1 (CDR(ASSOC 11 sentlist))) 
    (setq ap2 (CDR(ASSOC 10 sentlist))) 
    
    );p
  
;get last line end point if not reverse
  (progn
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 11 sentlist)))
(setq ap1 (CDR(ASSOC 10 sentlist)))
(setq ap2 (CDR(ASSOC 11 sentlist))) 
);p else

);if

(setq p1 (trans p1 0 1))
(setq ap1 (trans ap1 0 1))
  (setq ap2 (trans ap2 0 1))


(Setq op1 (getpoint "Select Side of line to draw arc:"))
  (command "erase" sent "")
  (setq offang (+ (angle ap1 ap2) (* 0.5 pi)))
  (if (> offang (* 2 pi))(setq offang (- offang (* 2 pi))))
  (setq op2 (polar op1 offang 50))
  (setq intpt (inters ap1 ap2 op1 op2 nil))
  (setq offangs (rtos (angle op1 intpt) 2 9))
  (if (= (rtos offang 2 9) offangs)(setq curverot "ccw")(setq curverot "cw"))
  (setq prevrot curverot)


  (SETQ MAST (SQRT (- (* (atof RADIUS) (atof RADIUS)) (* (/ (ATOF DIST) 2)(/ (ATOF DIST) 2 )))))
  (SETQ O (* 2 (ATAN (/ (/ (ATOF DIST) 2) MAST))))
  	   (setq arclength (rtos ( * (atof radius) O) 2 3))
  ;(if (= units "F")(setq arclength (rtos ( * (/ (atof radius) 0.3048) O) 2 3)))
  ;(if (= units "L")(setq arclength (rtos ( * (/ (atof radius) 0.201168) O) 2 3)))
	    (setq remhalfO (/ (- pi O) 2))
	    (if (= curverot "ccw")(setq raybearing (+ (angle ap1 ap2) remhalfO))(setq raybearing (- (angle ap1 ap2) remhalfO)))
	    
;round arclength
  (if (= qround "YES")
    (progn
      (SETQ ARCLENGTH (ATOF ARCLENGTH))
 (IF (< arclength DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> arclength DISTMAX1)(< arclength DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> arclength DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ arclength DROUND)))
    (SETQ LFP (- (/ arclength DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ arclength (* LIP DROUND))
    
    (SETQ ARCLENGTH (RTOS arclength 2 3))
    ;(IF (< arclength 1) (SETQ DTS (STRCAT "0" DTS)))
 ))


	    
	    (setq curvecen (polar ap1 raybearing  (atof radius)))
  	    (setq curvecenc (strcat (rtos (car curvecen) 2 6) "," (rtos (cadr curvecen) 2 6)))
  ;calc curve midpoint
  (setq a1 (angle curvecen ap1))
  (setq a2 (angle curvecen ap2))
  (if (= curverot "ccw")(setq da (- a2 a1))(setq da (- a1 a2)))
  (if (< da 0)(setq da (+ da (* 2 pi))))
    (SETQ DA (/ DA 2))
    (IF (= CURVEROT "ccw")(setq midb (+ a1 da))(setq midb (+ a2 da)))
  (setq amp (polar curvecen midb (atof radius)))
(IF ( = rswitch "T")(setq obearing (substr lbearing 2 200))(setq obearing lbearing))

    (if (= curverot "ccw") (command "arc" "c" curvecenc ap1 ap2)(command "arc" "c" curvecenc ap2 ap1))

  ;reverse arc direction if reversed
  (if (and (= rswitch "T")(= curverot "ccw"))(setq rcurverot "cw"))
  (if (and (= rswitch "T")(= curverot "cw"))(setq rcurverot "ccw"))
  (if (= rswitch  "T")(setq curverot rcurverot))

   ;LOOK FOR SPECIAL COMMENTS
  (setq arctype "" )
  (IF (OR (= (STRCASE comment) "AD")(= (STRCASE comment) "ADOPT"))(setq arctype " arcType=\"Adopt Dimension\" "  comment ""))
  (if (= (strcase (substr comment 1 3)) "AD ")(setq distancetype " arcType=\"Adopt Dimension\" "))
  (IF (OR (= (STRCASE comment) "COMP")(= (STRCASE comment) "COMPUTED"))(setq arctype " arcType=\"Computed\" "   comment ""))
  (if (= (strcase (substr comment 1 5)) "COMP ")(setq arctype " arcType=\"Computed\" "  ))
  (IF (OR (= (STRCASE comment) "D")(= (STRCASE comment) "DERIVED"))(setq arctype " arcType=\"Derived\" "  comment ""))
  (if (= (strcase (substr comment 1 8)) "DERIVED ")(setq arctype " arcType=\"Derived\" " ))
  (IF (OR (= (STRCASE comment) "MEAS")(= (STRCASE comment) "MEASURED"))(setq arctype " arcType=\"Measured\" "   comment ""))
  (if (= (strcase (substr comment 1 9)) "MEASURED ")(setq arctype " arcType=\"Measured\" " ))
  

  
  ;Add observation data to line as XDATA
      (if (/= comment "")(setq ocomment (strcat "><FieldNote>\"" comment "\"</FieldNote></ReducedArcObservation>"))(setq ocomment "/>"))
(SETQ BDINFO (STRCAT "chordAzimuth=\"" obearing "\" length=\"" arclength "\" radius=\"" radius  "\" rot=\"" curverot "\"" arctype ocomment))
 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
      


 
  (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))

 
  (setq tp1  (trans ap1 1 0))
  (setq tp2  (trans ap2 1 0))
  (setq amp  (trans amp 1 0))
  					   
  
   

 
   (LBARC);label arc using function
(setvar "clayer" prevlayer)
 
       
    (SETQ AANG (ANGLE tP2 tP1))
  

  


  
  ;GET 2ND+ LINE INFO
  (while (> 1 0) (progn
		   (setq bearing (getstring "\nChord Bearing [Last]: "))

        (setq dist (getstring T (strcat "\nChord Distance [Last/ArcDistance]" units ":")))

		   (if (or (= dist "a") (= dist "A") (= dist "ArcDistance") (= dist "arcdistance") (= DIST "ARCDISTANCE"))
    (progn
      (setq arcdist "Y")
      (setq dist (getstring T (strcat "\nArc Distance " units ":")))
      )
		     (setq arcdist "N"))
		   



		   
(setq radius (getstring (strcat "\nRadius [Last] (" units "):")))
		   (if (or (= bearing "Last") (= bearing "L") (= bearing "l") (= bearing "" )) (setq bearing lbearing))
		   (if (or (= dist "Last") (= dist "L") (= dist "l") (= dist "")) (setq dist prevdist))
		   (if (or (= radius "Last") (= radius "L") (= radius "l") (= radius "")) (setq radius prevradius))
(setq prevradius radius)
(setq prevdist dist)


;look for line comment
  (setq spcpos1 (vl-string-position 32 dist 0))
	(if (= spcpos1 nil)(setq comment "")(progn
					      (setq comment (substr dist ( + spcpos1 2) 50))
					      (setq dist (substr dist 1  spcpos1 ))
					      )
	  )

  
 ;APPLY ALL CORRECTIONS AND EXTRACT INFORMATION FROM USER INPUT


  ;reverse
  (if (or (= (substr bearing 1 1 ) "r") (= (substr bearing 1 1 ) "R" ))
    (progn
      (setq bearing (substr bearing 2 50))
      (setq rswitch "T")
      )
     (setq rswitch "F")
    )

;look for cardinals
  (setq card1 ""
	card2 "")
  (IF (OR (= (substr bearing 1 1 ) "n") (= (substr bearing 1 1 ) "N" )
	  (= (substr bearing 1 1 ) "s" )(= (substr bearing 1 1 ) "S" )
	  (= (substr bearing 1 1 ) "e" )(= (substr bearing 1 1 ) "E" )
	  (= (substr bearing 1 1 ) "w" )(= (substr bearing 1 1 ) "W" ))
(progn
    (setq card1 (substr bearing 1 1))
    (setq card2 (substr bearing (strlen bearing) 1))
    (setq bearing (substr bearing 2 (- (strlen bearing )2)))
  )
    )
    
  
(if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
  (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins  (strcat (substr bearing (+ dotpt1 2) 2) (chr 39)))
  (setq sec  (substr bearing (+ dotpt1 4) 10))

  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))

    (if (or
	(= (strlen sec) 2)
	(= (strlen mins) 2)
	(> (atof mins) 60)
	(> (atof sec) 60)
	(> (atof deg) 360)
	)
    (alert (strcat "That bearing looks a little funky - " bearing)))
  
  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

 ;correct cardinals

		     (IF  (and (or (= card1 "n")(= card1 "N")(= card1 "s")(= card1 "S"))(and (/= card2 "w")(/= card2 "W")(/= card2 "e")(/= card2 "E")))
	  (alert (strcat "Cardinal missing E or W"))
    )
		   
  (if (and (or (= card1 "n")(= card1 "N"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (- 360 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );NW

		   

		   (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (- 270 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );WS

		   
		   (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (- 90 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );EN

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "e")(= card2 "E")))
    (progn
      (setq deg (rtos (- 180 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );SE

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (+ 180 (atof deg)) 2 0))
      
      )
    );SW

		    (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (+ 270 (atof deg)) 2 0))
      
      )
    );WN
		    (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (+ 90 (atof deg)) 2 0))
      
      )
    );ES
  
	
 		   (if (and (= mins "")(= sec ""))(setq decimal "")(setq decimal "."))
	(setq lbearing (strcat deg decimal (substr mins 1 2) (substr sec 1 2)))
 (IF (= rswitch "T")(setq lbearing (strcat "R" lbearing)))
  
		 
		   
(setq bearing (strcat  deg "d" mins sec))

 

  
    (if (= units "F")
      (progn
	(setq dotpos1 (vl-string-position 46 dist 0))
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 dist (+ dotpos1 1))))
	(setq /pos1 (vl-string-position 47 dist 0))
	(if (/= /pos1 nil)
	  (progn
	    (setq den (substr dist ( + /pos1 2) 50))
	    (setq num (substr dist ( + dotpos2 2) (- (- /pos1 dotpos2) 1)))
	    (setq idist (/ (atof num) (atof den)))
	    (setq inches (substr dist (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq idist (+ idist (atof inches)))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (/= dotpos1 nil) (= /pos1 nil))
	  (progn
	    (setq inches (substr dist ( + dotpos1 2) 50))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist  (atof inches))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (= dotpos1 nil) (= /pos1 nil) (= dotpos2 nil))
	  (progn
	   
	    (setq feet (substr dist 1  50))
	    (setq idist (* (atof feet) 12))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
;radius in feet
	
	(setq dotpos1 (vl-string-position 46 radius 0))
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 radius (+ dotpos1 1))))
	(setq /pos1 (vl-string-position 47 radius 0))
	(if (/= /pos1 nil)
	  (progn
	    (setq den (substr radius ( + /pos1 2) 50))
	    (setq num (substr radius ( + dotpos2 2) (- (- /pos1 dotpos2) 1)))
	    (setq idist (/ (atof num) (atof den)))
	    (setq inches (substr radius (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq idist (+ idist (atof inches)))
	    (setq feet (substr radius 1  dotpos1 ))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq radius (rtos (* idist 0.0254)))
	    )
	  )
	(if (and (/= dotpos1 nil) (= /pos1 nil))
	  (progn
	    (setq inches (substr radius ( + dotpos1 2) 50))
	    (setq feet (substr radius 1  dotpos1 ))
	    (setq idist  (atof inches))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq radius (rtos (* idist 0.0254)))
	    )
	  )
	(if (and (= dotpos1 nil) (= /pos1 nil) (= dotpos2 nil))
	  (progn
	   
	    (setq feet (substr radius 1  50))
	    (setq idist (* (atof feet) 12))
	    (setq radius (rtos (* idist 0.0254)))
	    )
	  )
	

	
	));if & p feet

  
  (if (= units "L")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.201168)))
      (setq radius (atof radius))
      (setq radius (rtos (* radius 0.201168)))
      
      )); if & p links
 (if (= units "DF")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.3048)))
      (setq radius (atof radius))
      (setq radius (rtos (* radius 0.3048)))
      
      )); if & p decimal feet


(if (= arcdist "Y")(setq dist (rtos (* (* 2 (atof radius)) (sin (/ (atof dist) (* 2 (atof radius))))))))
	      

    ;DRAW LINE 1
  (setq dist (rtos (atof dist)2 3));remove trailing zeros
  (setq ldist dist)
  (setq radius (rtos (atof radius)2 3));remove trailing zeros
  (setq lradius radius)
		   
  
  (if (= dist "0") (progn
		     (princ "\nDistance of 0 entered")
		     (exit))
    )
		   

  (setq linetext (strcat "@" dist "<" bearing))
 
  (command "line" p1 linetext "")
		   (SETVAR "CLAYER" prevlayer)

  
  
;Add observation data to line as XDATA

;Move line if reverse activated
(if (= rswitch "T")
  (progn
    (SETQ SENT (ENTLAST))
    (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 10 sentlist)))
    (setq p1 (CDR(ASSOC 10 sentlist)))
    (setq p2 (CDR(ASSOC 11 sentlist)))
    (command "move" sent "" p2 p1)

;get last line end point
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 10 sentlist)))
    (setq ap1 (CDR(ASSOC 11 sentlist))) 
    (setq ap2 (CDR(ASSOC 10 sentlist))) 
    
    );p
  
;get last line end point if not reverse
  (progn
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 11 sentlist)))
(setq ap1 (CDR(ASSOC 10 sentlist)))
(setq ap2 (CDR(ASSOC 11 sentlist))) 
);p else

);if

		   (setq p1 (trans p1 0 1))
(setq ap1 (trans ap1 0 1))
  (setq ap2 (trans ap2 0 1))

(Setq op1 (getpoint "Select Side of line to draw arc (Last):"))
		   (command "erase" sent "")
		   (if (= op1 nil)(setq curverot prevrot)(progn
		   
  (setq offang (+ (angle ap1 ap2) (* 0.5 pi)))
  (if (> offang (* 2 pi))(setq offang (- offang (* 2 pi))))
  (setq op2 (polar op1 offang 50))
  (setq intpt (inters ap1 ap2 op1 op2 nil))
  (setq offangs (rtos (angle op1 intpt) 2 9))
  (if (= (rtos offang 2 9) offangs)(setq curverot "ccw")(setq curverot "cw"))
		   ))
(setq prevrot curverot)

  (SETQ MAST (SQRT (- (* (atof RADIUS) (atof RADIUS)) (* (/ (ATOF DIST) 2)(/ (ATOF DIST) 2 )))))
  (SETQ O (* 2 (ATAN (/ (/ (ATOF DIST) 2) MAST))))
  	   (setq arclength (rtos ( * (atof radius) O) 2 3))
           ;(if (= units "F")(setq arclength (rtos ( * (/ (atof radius) 0.3048) O) 2 3)))
           ;(if (= units "L")(setq arclength (rtos ( * (/ (atof radius) 0.201168) O) 2 3)))
	    (setq remhalfO (/ (- pi O) 2))
	    (if (= curverot "ccw")(setq raybearing (+ (angle ap1 ap2) remhalfO))(setq raybearing (- (angle ap1 ap2) remhalfO)))
	    
	    (setq curvecen (polar ap1 raybearing  (atof radius)))
	    (setq curvecenc (strcat (rtos (car curvecen) 2 6) "," (rtos (cadr curvecen) 2 6)))
		   ;calc curve midpoint
  (setq a1 (angle curvecen ap1))
  (setq a2 (angle curvecen ap2))
  (if (= curverot "ccw")(setq da (- a2 a1))(setq da (- a1 a2)))
  (if (< da 0)(setq da (+ da (* 2 pi))))
    (SETQ DA (/ DA 2))
    (IF (= CURVEROT "ccw")(setq midb (+ a1 da))(setq midb (+ a2 da)))
  (setq amp (polar curvecen midb (atof radius)))


    (if (= curverot "ccw") (command "arc" "c" curvecenc ap1 ap2)(command "arc" "c" curvecenc ap2 ap1))
  (IF ( = rswitch "T")(setq obearing (substr lbearing 2 200))(setq obearing lbearing))


		    ;reverse arc direction if reversed
  (if (and (= rswitch "T")(= curverot "ccw"))(setq rcurverot "cw"))
  (if (and (= rswitch "T")(= curverot "cw"))(setq rcurverot "ccw"))
  (if (= rswitch  "T")(setq curverot rcurverot))


		   
;round arclength
  (if (= qround "YES")
    (progn
      (SETQ ARCLENGTH (ATOF ARCLENGTH))
 (IF (< arclength DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> arclength DISTMAX1)(< arclength DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> arclength DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ arclength DROUND)))
    (SETQ LFP (- (/ arclength DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ arclength (* LIP DROUND))
    
    (SETQ ARCLENGTH (RTOS arclength 2 3))
    ;(IF (< arclength 1) (SETQ DTS (STRCAT "0" DTS)))
 ))

  ;LOOK FOR SPECIAL COMMENTS
  (setq arctype "" )
  (IF (OR (= (STRCASE comment) "AD")(= (STRCASE comment) "ADOPT"))(setq arctype " arcType=\"Adopt Dimension\" "  comment ""))
  (if (= (strcase (substr comment 1 3)) "AD ")(setq distancetype " arcType=\"Adopt Dimension\" "))
  (IF (OR (= (STRCASE comment) "COMP")(= (STRCASE comment) "COMPUTED"))(setq arctype " arcType=\"Computed\" "   comment ""))
  (if (= (strcase (substr comment 1 5)) "COMP ")(setq arctype " arcType=\"Computed\" "  ))
  (IF (OR (= (STRCASE comment) "D")(= (STRCASE comment) "DERIVED"))(setq arctype " arcType=\"Derived\" "  comment ""))
  (if (= (strcase (substr comment 1 8)) "DERIVED ")(setq arctype " arcType=\"Derived\" " ))
  (IF (OR (= (STRCASE comment) "MEAS")(= (STRCASE comment) "MEASURED"))(setq arctype " arcType=\"Measured\" "   comment ""))
  (if (= (strcase (substr comment 1 9)) "MEASURED ")(setq arctype " arcType=\"Measured\" " ))
  

		   
    (if (/= comment "")(setq ocomment (strcat "><FieldNote>\"" comment "\"</FieldNote></ReducedArcObservation>"))(setq ocomment "/>"))
(SETQ BDINFO (STRCAT "chordAzimuth=\"" obearing "\" length=\"" arclength "\" radius=\"" radius  "\" rot=\"" curverot "\"" arcType ocomment))
 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
 

 
  (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))

  (setq tp1  (trans ap1 1 0))
  (setq tp2  (trans ap2 1 0))
  (setq amp  (trans amp 1 0))
   

 
   (LBARC);label arc using function
(setvar "clayer" prevlayer)
 
       
    (SETQ AANG (ANGLE tP2 tP1))
  

		   
  
);P
    );WHILE 1>0
  );DEFUN



;----------------------------------------------------------------CREATE LOT--------------------------------------------

(defun C:XCL (/)

  (setq prevlayer (getvar "CLAYER"))
  (SETQ areapercent NIL)
  (setq pickstyle (getvar "Pickstyle"))
  (setvar "pickstyle" 0)

  (SETVAR "CLAYER"  "Lot Definitions" )
  (setq lotc (getpoint "\nSelect Lot Centre:"))
  
  (command "-layer" "off" "Drafting"
	   "off" "Drafting AFR"
	   "off" "Easement"
	   "off" "Traverse"
	   "off" "Sideshot"
	   "off" "Topo"
	   "off" "Occupation Fences"
	   "Off" "Occupation Walls"
	   "off" "Occupation Building Return"
	   "off" "Occupation Timber Walls"
	   "off" "Occupation Fences"
	   "off" "Occupation Centreline"
	   "off" "Occupation Railway"
	   "off" "Occupation Rockwall"
	   "off" "Occupation Hedge"
	   "off" "Occupation Other"
	   "off" "Occupations"
	   "off" "Occupation Kerbs"
	   "off" "Occupation Buildings"
	   "off" "Occupation Gate"
	   "off" "Occupation Not Fenced" ""
	   )

  
  (command "-hatch" "A" "I" "N" "N" "" "p" "s" lotc "")
  (command "-layer" "on" "Drafting"
	 	   "on" "Easement"
	   "on" "Drafting AFR"
	   "on" "Traverse"
	   "on" "Sideshot"
	   "on" "Topo"
	   "on" "Occupation Fences"
	   "on" "Occupation Walls"
	   "on" "Occupation Building Return"
	   "on" "Occupation Timber Walls"
	   "on" "Occupation Fences"
	   "on" "Occupation Centreline"
	   "on" "Occupation Railway"
	   "on" "Occupation Rockwall"
	   "on" "Occupation Hedge"
	   "on" "Occupation Other"
	   "on" "Occupations"
	   "on" "Occupation Kerbs"
	   "on" "Occupation Buildings"
	   "on" "Occupation Gate"
	   "on" "Occupation Not Fenced" ""
	   
	   )
  (SETQ lothatch (ENTLAST))
  (SETQ ENTSS lothatch)
(command "._HATCHGENERATEBOUNDARY" ENTSS "");edited for Bricscad, much neater, courtesy Jason Bourhill www.cadconcepts.co.nz
  (COMMAND "SELECT" "" "")
  (COMMAND "ERASE" ENTSS "")
  (SETQ lotedge (ENTLAST))
  (SETQ ENTSS (SSADD))
  (SSADD lotedge ENTSS)

  (if (= plotno nil) (setq plotno "1"))
  
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
 (SETQ lotno (getstring T (strcat "\n Lot Number [" plotno "]:" )))
  (if (= lotno "") (setq lotno plotno))
       
  (setq area (getstring "\nArea or [C]alculate (mm.dm) (aa.rr.pp.f/p) [Last]:"))
(if (or (= area "")(= area "l")(= area "L")(= area "LAST")(= area "last"))(setq area "Last"))

  (if (= area "Last" )(setq area arealast))
  (SETQ arealast area)


  ;deal with imperial areas
  
      
	(setq dotpos1 (vl-string-position 46 area 0))
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 area (+ dotpos1 1))))
	(if (/= dotpos2 nil)(progn;idenfited as imperial area, must have second dotpos to work
			      
	(if (= dotpos2 nil)(setq dotpos3 nil)(setq dotpos3 (vl-string-position 46 area (+ dotpos2 1))))
	(setq /pos1 (vl-string-position 47 area 0))
	(if (/= /pos1 nil);with factional part
	  (progn
	    (setq den (substr area ( + /pos1 2) 50))
	    (setq num (substr area ( + dotpos3 2) (- (- /pos1 dotpos3) 1)))
	    (setq fperch (/ (atof num) (atof den)))
	    (setq perch (substr area (+ dotpos2 2) (- (- dotpos3 dotpos2) 1)))
	    (setq perch (+ fperch (atof perch)))
	    (setq rood (substr area (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq perch (+ perch (* (atof rood) 40)))
	    (setq acre (substr area 1  dotpos1 ))
	    (setq perch (+ perch (* (atof acre) 160)))
	    (setq area (rtos (* perch 25.2929538117) 2 9))
	    )
	  )
	(if (and (/= dotpos1 nil)(/= dotpos2 nil)(= /pos1 nil));without fractional part
	  (progn
	    (setq perch (substr area ( + dotpos2 2) 50))
	    (setq perch (atof perch))
	    (setq rood (substr area (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq perch (+ perch (* (atof rood) 40)))
	    (setq acre (substr area 1  dotpos1 ))
	    (setq perch (+ perch (* (atof acre) 160)))
	    (setq area (rtos (* perch 25.2929538117) 2 9))
	    )
	  )
	
	));p&if imperial area
	  

	

  




  
   
   (SETQ area1 (vlax-get-property (vlax-ename->vla-object sent ) 'area ))

  ;(setvar "dimzin" 0)
  (IF (or ( = area "C")(= area "c"))
    (progn
      (setq area (rtos area1 2 3))
      (setq area1 (atof (rtos area1 2 3)));deal with recurring 9's
      					    (if (> area1 0)(setq textarea (strcat (rtos (*  (/ area1 0.1) 0.1) 2 1) "m²")))
					    (if (> area1 100)(setq textarea (strcat (rtos (*  (/ area1 1) 1) 2 0) "m²")))
      					    (if (> area1 10000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 0.001) 0.001) 2 3) "ha")))
					    (if (> area1 100000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 0.01) 0.01) 2 2) "ha")))
					    (if (> area1 1000000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 0.1) 0.1) 2 1) "ha")))
					    (if (> area1 10000000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 1) 1) 2 0) "ha")))
                                            (if (> area1 100000000) (setq textarea (strcat (rtos (*  (/ (/ area1 1000000) 0.1) 0.1) 2 1) "km²")))
                                            (if (> area1 1000000000) (setq textarea (strcat (rtos (*  (/ (/ area1 1000000) 1) 1) 2 0) "km²")))
      
      
					    
      )
    (progn
     (setq areapercent (ABS(* (/  (- area1 (ATOF area)) area1) 100)))
     (if (> areapercent 10) (alert (strcat "\nArea different to calulated by " (rtos areapercent 2 0)"%")))

      (if (> (atof area) 1)(setq textarea (strcat (rtos (atof area) 2 3) "m²")))
 					    (if (> (atof area) 0)(setq textarea (strcat (rtos (* (/ (atof area) 0.1) 0.1) 2 1) "m²")))
					    (if (> (atof area) 100)(setq textarea (strcat (rtos (* (/ (atof area) 1) 1) 2 0) "m²")))
      					    (if (> (atof area) 10000) (setq textarea (strcat (rtos (* (/ (/ (atof area) 10000) 0.001) 0.001) 2 3) "ha")))
					    (if (> (atof area) 100000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 0.01) 0.01) 2 2) "ha")))
					    (if (> (atof area) 1000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 0.1) 0.1) 2 1) "ha")))
					    (if (> (atof area) 10000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 1) 1) 2 0) "ha")))
                                            (if (> (atof area) 100000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 1000000) 0.1) 0.1) 2 1) "km²")))
                                            (if (> (atof area) 1000000000) (setq textarea (strcat (rtos (* (/ (/ (atof area) 1000000) 1) 1) 2 0) "km²")))

     )
    )
  ;(setvar "dimzin" 8)
    (setq lotstate (getstring "\nCreated/Extinguished/Affected/eXisting (C/E/A/X) [C]:"))
  (if (or (= lotstate "c")(= lotstate "C"))(setq lotstate "created"))
  (if (or (= lotstate "e")(= lotstate "E"))(setq lotstate "extinguished"))
  (if (or (= lotstate "a")(= lotstate "A"))(setq lotstate "affected"))
  (if (or (= lotstate "x")(= lotstate "X"))(setq lotstate "existing"))
  (if (= lotstate "")(setq lotstate "created"))

  (if (= (substr lotno 1 2) "PT")(setq pcltype " parcelType=\"Part\""
				     lotno (substr lotno 3 50)
				       ;desc "\" desc=\"PT"
				       textarea (strcat "(" textarea ")")
				       )
    (setq pcltype " parcelType=\"Single\""
	  desc ""    
    ))
(setq lotc (trans lotc 1 0));convert to world if in a UCS

  (if (= (substr lotno 1 2) "CM")(setq oclass "Common Property")(setq oclass "Lot"))
  
    ;<Parcel name="30" class="Lot" state="proposed" parcelType="Single" parcelFormat="Standard" area="951.8">
  (SETQ LTINFO (STRCAT "  <Parcel name=\"" lotno "\" class=\"" oclass "\" state=\"" lotstate "\"" pcltype "  area=\""
		       area "\">!" (rtos (cadr lotc) 2 6 ) " " (rtos (car lotc) 2 6)))
	 ;(setq arealast area)
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

  (if (= pcltype " parcelType=\"Part\"")(setq lotnos (strcat "PT" lotno))(setq lotnos lotno))
  
         
       (setq lotc (trans lotc 0 1));convert back to UCS if using one
(SETVAR "CELWEIGHT" 50)
  (if (= lotstate "extinguished")(setvar "clayer" "Drafting AFR")(SETVAR "CLAYER"  "Drafting" ))
  (setq areapos (polar lotc (* 1.5 pi) (* th 2.5)))
  (COMMAND "TEXT" "J" "BC" lotc (* TH 2) "90" lotnos)
  (SETVAR "CELWEIGHT" 35)
  (COMMAND "TEXT" "J" "BC" areapos (* TH 1.4) "90"  textarea )
(SETVAR "CELWEIGHT" -1)
  (PRINC (STRCAT "LOT:" LOTNO))
  (if (/= (setq stringpos (vl-string-search "~" lotno )) nil)(setq suffix (substr lotno (+ stringpos 1)))(setq suffix ""))
  (if (/= (atof lotno) 0)(setq plotno (strcat (rtos (+ (atof lotno) 1) 2 0) suffix)))  
  (SETVAR "CLAYER" prevlayer)

  
  (COMMAND "DRAWORDER" ENTSS "" "BACK")

  (setvar "pickstyle" pickstyle)
  (IF (/= areapercent NIL)(PRINC (strcat "\nArea different to calulated by " (rtos areapercent 2 0)"%")))
  
  )

;-----------------------------------------------------------------------------Create Easement Geometry----------------------------------------------------
(defun C:XCE (/)

  (setq pickstyle (getvar "Pickstyle"))
  (setvar "pickstyle" 0)
  (setq prevlayer (getvar "CLAYER"))
 (easecount)
  (SETVAR "CLAYER"  "Lot Definitions" )
  (setq lotc (getpoint "\nSelect Easement Centre:"))
  (command "-layer" "off" "Drafting"
	   "off" "Drafting AFR"
	   ;"off" "Easement"
	   "off" "Traverse"
	   "off" "Sideshot"
	   "off" "Topo"
	   "off" "Occupation Fences"
	   "Off" "Occupation Walls"
	   "off" "Occupation Building Return"
	   "off" "Occupation Timber Walls"
	   "off" "Occupation Fences"
	   "off" "Occupation Centreline"
	   "off" "Occupation Railway"
	   "off" "Occupation Rockwall"
	   "off" "Occupation Hedge"
	   "off" "Occupation Other"
	   "off" "Occupations"
	   "off" "Occupation Kerbs"
	   "off" "Occupation Buildings"
	   "off" "Occupation Gate"
	   "off" "Occupation Not Fenced" ""
	   )

  
  (command "-hatch" "A" "I" "N" "N" "" "p" "s" lotc "")
  (command "-layer" "on" "Drafting"
	   ;"on" "Easement"
	   "on" "Drafting AFR"
	   "on" "Traverse"
	   "on" "Sideshot"
	   "on" "Topo"
	   "on" "Occupation Fences"
	   "on" "Occupation Walls"
	   "on" "Occupation Building Return"
	   "on" "Occupation Timber Walls"
	   "on" "Occupation Fences"
	   "on" "Occupation Centreline"
	   "on" "Occupation Railway"
	   "on" "Occupation Rockwall"
	   "on" "Occupation Hedge"
	   "on" "Occupation Other"
	   "on" "Occupations"
	   "on" "Occupation Kerbs"
	   "on" "Occupation Buildings"
	   "on" "Occupation Gate"
	   "on" "Occupation Not Fenced" ""
	   )
	   
  (SETQ lothatch (ENTLAST))
  (SETQ ENTSS (SSADD))
  (SSADD lothatch ENTSS)
(command "._HATCHGENERATEBOUNDARY" ENTSS "");edited for Bricscad, much neater, courtesy Jason Bourhill www.cadconcepts.co.nz
(COMMAND "SELECT" "" "")
  (COMMAND "ERASE" ENTSS "")
  (SETQ lotedge (ENTLAST))
  (SETQ ENTSS (SSADD))
  (SSADD lotedge ENTSS)

  
  
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))

  (SETQ PTLIST (LIST))
				
;CREATE LIST OF POLYLINE POINTS
    (SETQ PTLIST (LIST))
	    (foreach a SENTLIST
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	     
	    )				;FOREACH 			)

(setq ptlist (append ptlist (list(nth 0 ptlist))))

   
 (SETQ easeid (getstring T (STRCAT "\nEasment Identifier (E# or PTE#):")))
  (if (or (= (substr easeid 1 2) "PT")(= (substr easeid 1 2) "pt")(= (substr easeid 1 2) "Pt"))(setq easeid (substr easeid 3)
												     parceltype "Part")
    (setq parceltype "Single")
    )
  
  
 (SETQ lotstate (getstring T (STRCAT "\n Created/Existing [C/E](defult is created):")))
  (if (or (= (strcase lotstate) "C")(= (strcase lotstate) "CREATED")(= lotstate ""))(setq lotstate "created"))
  (if (or (= (strcase lotstate) "E")(= (strcase lotstate) "EXISTING"))(setq lotstate "existing"))

  (if (= (substr easeid 1 3) "RST")
    (SETQ pclclass "Restriction"
	  pclformat "")
    (setq pclclass "Easement";else normal easement
	  pclformat " parcelFormat=\"Geometry\" ")
    )
   
 
    (setq lotc (trans lotc 1 0));convert to world if using UCS
  (SETQ LTINFO (STRCAT "<Parcel name=\"" easeid "\""  " class=\"" pclclass "\" state=\"" lotstate "\" parcelType=\"" parceltype "\"" pclformat  ">!"
		       (rtos (cadr lotc) 2 6 ) " " (rtos (car lotc) 2 6)))
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

  (setq THF 1)

  (setq lotc (trans lotc 0 1));convert back to UCS

  (SETVAR "CLAYER"  "Drafting" )

  		      
					
  (COMMAND "TEXT" "J" "MC" lotc (* TH THF) "90" easeid )
       (setq roadname (entget (entlast)))

   (SETVAR "CLAYER" prevlayer)


(setq lotc (trans lotc 1 0));convert to world if using UCS
    
		(setq count 0)
		(setq minoff 100000000000000000000000000000000000000)
(repeat (- (length ptlist)2 )

  (setq op1  (nth count ptlist) )
  (setq op2  (nth (+ count 1) ptlist))
  (setq op3  (nth (+ count 2) ptlist))



  ;check line one
;check offset to line
  (SETQ ANG (ANGLE oP1 oP2))
  (SETQ CANG (+ ANG (/ PI 2)))
  (SETQ P4 (POLAR lotc CANG 50))
  (SETQ P6 (POLAR lotc (+ CANG PI) 50))
   
  (IF (SETQ P5 (INTERS oP1 oP2 P6 P4 ))(PROGN
					
 
      (SETQ OFF (DISTANCE lotc P5))

    (if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang cang))
     
    
  ));p & if inters

    ;check inside deflection angle
    (setq ang2 (angle op2 op3))
    (setq cang2 (+ ang (/ pi 2)))
    (setq ltopang (angle op2 lotc))
    (setq defl (- ang2 ang))
(if (/= defl 0);check for straight line
  (progn
    (if (< defl pi)(progn
		     (setq ray1 (- ang (/ pi 2)))
		     ;(if (< ray1 0) (setq ray1 (+ ray1 (* 2 pi))))
		     (setq ray2 (- ang2 (/ pi 2)))
		     ;(if (< ray2 0) (setq ray2 (+ ray2 (* 2pi))))
		     ));p and if less than pi
    (if (> defl pi)(progn
		     (setq ray1 (+ ang (/ pi 2)))
		     ;(if (> ray1 (* 2 pi)(setq ray1 (- ray1 (* 2 pi)))))
		     (setq ray2 (+ ang (/ pi 2)))
		     ;(if (> ray2 (* 2 pi)(setq ray2 (- ray2 (* 2 pi)))))
		     ));p and if greater than pi

    (if (or (and (> ltopang ray1) (< ltopang ray2))(and (> ltopang ray2)(< ltopang ray1)));check ot see if inside deflection squares **** needs testing later
      (progn
	(setq off (distance lotc op2))
(if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang ltopang)
      );if
	));p and if in defl sqr
   ));p and if not straight line

  (setq count (+ count 1))
    );repeat
      
				;using this info change the road name text rotation to the angle of the line
		(setq rrot (+ nearang (/ pi 2)))
		(if (> rrot (* 2 pi))(setq rrot (- rrot (* 2 pi))))
		(IF (AND (> rrot  (* 0.5 pi)) (< rrot (* 1.5 pi)))(setq rrot (+ rrot pi))) ;if text is upsidedown reverse rotation
		;check and reverse if upside down in OCS
  (setq p1 (polar lotc rrot 10))
  (setq llotc (trans lotc 0 1))
  (setq lp1 (trans p1 0 1))
  (setq oang (angle llotc lp1))
  (IF (AND (> oang  (* 0.5 pi)) (< oang (* 1.5 pi)))(setq rrot (+ rrot pi))) ;if text is upsidedown reverse rotation
  
		
    
  (SETQ	roadname (subst (cons 50  rrot);SUB IN rotation
		     (assoc 50 roadname)
		     roadname     ) )
  (ENTMOD roadname)
(COMMAND "DRAWORDER" ENTSS "" "BACK")

   
  (setvar "pickstyle" pickstyle)
  )



;-----------------------------------------------------------------------------Create Restriction Geometry----------------------------------------------------
(defun C:XCRE (/)

  (setq pickstyle (getvar "Pickstyle"))
  (setvar "pickstyle" 0)
  (setq prevlayer (getvar "CLAYER"))
 (easecount)
  (SETVAR "CLAYER"  "Lot Definitions" )
  (setq lotc (getpoint "\nSelect Restriction Centre:"))
  (command "-layer" "off" "Drafting"
	   
	   "off" "Drafting AFR"
	   "off" "Traverse"
	   "off" "Sideshot"
	   "off" "Occupation Fences"
	   "Off" "Occupation Walls"
	   "off" "Occupation Building Return"
	   "off" "Occupation Timber Walls"
	   "off" "Occupation Fences"
	   "off" "Occupation Centreline"
	   "off" "Occupation Railway"
	   "off" "Occupation Rockwall"
	   "off" "Occupation Hedge"
	   "off" "Occupation Other"
	   "off" "Occupations"
	   "off" "Occupation Kerbs"
	   "off" "Occupation Buildings"
	   "off" "Occupation Gate"
	   "off" "Occupation Not Fenced" ""
	   )

  
  (command "-hatch" "A" "I" "N" "N" "" "p" "s" lotc "")
  (command "-layer" "on" "Drafting"
	 	   "on" "Easement"
	   "on" "Drafting AFR"
	   "on" "Traverse"
	   "on" "Sideshot"
	   "on" "Topo"
	   "on" "Occupation Fences"
	   "on" "Occupation Walls"
	   "on" "Occupation Building Return"
	   "on" "Occupation Timber Walls"
	   "on" "Occupation Fences"
	   "on" "Occupation Centreline"
	   "on" "Occupation Railway"
	   "on" "Occupation Rockwall"
	   "on" "Occupation Hedge"
	   "on" "Occupation Other"
	   "on" "Occupations"
	   "on" "Occupation Kerbs"
	   "on" "Occupation Buildings"
	   "on" "Occupation Gate"
	   "on" "Occupation Not Fenced" ""
	   )
	   
  (SETQ lothatch (ENTLAST))
  (SETQ ENTSS (SSADD))
  (SSADD lothatch ENTSS)
(command "._HATCHGENERATEBOUNDARY" ENTSS "");edited for Bricscad, much neater, courtesy Jason Bourhill www.cadconcepts.co.nz
(COMMAND "SELECT" "" "")
  (COMMAND "ERASE" ENTSS "")
  (SETQ lotedge (ENTLAST))
  (SETQ ENTSS (SSADD))
  (SSADD lotedge ENTSS)

  
  
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))

  (SETQ PTLIST (LIST))
				
;CREATE LIST OF POLYLINE POINTS
    (SETQ PTLIST (LIST))
	    (foreach a SENTLIST
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	     
	    )				;FOREACH 			)

(setq ptlist (append ptlist (list(nth 0 ptlist))))
  
  
 (SETQ lotstate (getstring T (STRCAT "\n Created/Existing [C/E](defult is created):")))
  (if (or (= (strcase lotstate) "C")(= (strcase lotstate) "CREATED")(= lotstate ""))(setq lotstate "created"))
  (if (or (= (strcase lotstate) "E")(= (strcase lotstate) "EXISTING"))(setq lotstate "existing"))

  (setq pclname (getstring "Restriction number:"))

  ;look for parts
   (if (= (substr pclname 1 2) "PT")(setq pclname (substr pclname 3)
					  singlepart "Part")
     (setq singlepart "Single"))
							  
 
    (setq lotc (trans lotc 1 0));convert to world if using UCS
  (SETQ LTINFO (STRCAT "<Parcel name=\"RST" pclname "\""  "\" class=\"Restriction\" state=\"" lotstate "\" parcelType=\"" Singlepart "\" >"))
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

  (setq THF 1)

  (setq lotc (trans lotc 0 1));convert back to UCS

  (SETVAR "CLAYER"  "Drafting" )

  		      
					
  (COMMAND "TEXT" "J" "MC" lotc (* TH THF) "90" (strcat "RST" pclname ))
       (setq roadname (entget (entlast)))

   (SETVAR "CLAYER" prevlayer)


(setq lotc (trans lotc 1 0));convert to world if using UCS
    
		(setq count 0)
		(setq minoff 100000000000000000000000000000000000000)
(repeat (- (length ptlist)2 )

  (setq op1 (nth count ptlist))
  (setq op2 (nth (+ count 1) ptlist))
  (setq op3 (nth (+ count 2) ptlist))



  ;check line one
;check offset to line
  (SETQ ANG (ANGLE oP1 oP2))
  (SETQ CANG (+ ANG (/ PI 2)))
  (SETQ P4 (POLAR lotc CANG 50))
  (SETQ P6 (POLAR lotc (+ CANG PI) 50))
   
  (IF (SETQ P5 (INTERS oP1 oP2 P6 P4 ))(PROGN
					
 
      (SETQ OFF (DISTANCE lotc P5))

    (if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang cang))
     
    
  ));p & if inters

    ;check inside deflection angle
    (setq ang2 (angle op2 op3))
    (setq cang2 (+ ang (/ pi 2)))
    (setq ltopang (angle op2 lotc))
    (setq defl (- ang2 ang))
(if (/= defl 0);check for straight line
  (progn
    (if (< defl pi)(progn
		     (setq ray1 (- ang (/ pi 2)))
		     ;(if (< ray1 0) (setq ray1 (+ ray1 (* 2 pi))))
		     (setq ray2 (- ang2 (/ pi 2)))
		     ;(if (< ray2 0) (setq ray2 (+ ray2 (* 2pi))))
		     ));p and if less than pi
    (if (> defl pi)(progn
		     (setq ray1 (+ ang (/ pi 2)))
		     ;(if (> ray1 (* 2 pi)(setq ray1 (- ray1 (* 2 pi)))))
		     (setq ray2 (+ ang (/ pi 2)))
		     ;(if (> ray2 (* 2 pi)(setq ray2 (- ray2 (* 2 pi)))))
		     ));p and if greater than pi

    (if (or (and (> ltopang ray1) (< ltopang ray2))(and (> ltopang ray2)(< ltopang ray1)));check ot see if inside deflection squares **** needs testing later
      (progn
	(setq off (distance lotc op2))
(if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang ltopang)
      );if
	));p and if in defl sqr
   ));p and if not straight line

  (setq count (+ count 1))
    );repeat
      
				;using this info change the road name text rotation to the angle of the line
		(setq rrot (+ nearang (/ pi 2)))
		(if (> rrot (* 2 pi))(setq rrot (- rrot (* 2 pi))))
		(IF (AND (> rrot  (* 0.5 pi)) (< rrot (* 1.5 pi)))(setq rrot (+ rrot pi))) ;if text is upsidedown reverse rotation
					  
		  (setq p1 (polar lotc rrot 10))
  (setq llotc (trans lotc 0 1))
  (setq lp1 (trans p1 0 1))
  (setq oang (angle llotc lp1))
  (IF (AND (> oang  (* 0.5 pi)) (< oang (* 1.5 pi)))(setq rrot (+ rrot pi))) ;if text is upsidedown reverse rotation
    
  (SETQ	roadname (subst (cons 50  rrot);SUB IN NEW POINT 2 HEIGHT
		     (assoc 50 roadname)
		     roadname     ) )
  (ENTMOD roadname)
(COMMAND "DRAWORDER" ENTSS "" "BACK")

   
  (setvar "pickstyle" pickstyle)
  )




;-----------------------------------------------------------------------------Create Easement Link point----------------------------------------------------
(defun C:XLE (/)
(setq pickstyle (getvar "Pickstyle"))
  (setvar "pickstyle" 0)

  (setvar "CLAYER" "lot definitions")
  (setq enamelist (list))
  
 (setq ename (getstring "Easement Identifiers (comma sep (or enter to Select polygons):" T))
  (if (= ename "")(progn
      (command "-layer" "off" "Drafting"
	       "off" "Boundary"
	       "off" "Boundary extinguished"
	   "off" "Drafting AFR"
	   "off" "Easement"
	   "off" "Traverse"
	   "off" "Sideshot"
	   "off" "Topo"
	   "off" "Occupation Fences"
	   "Off" "Occupation Walls"
	   "off" "Occupation Building Return"
	   "off" "Occupation Timber Walls"
	   "off" "Occupation Fences"
	   "off" "Occupation Centreline"
	   "off" "Occupation Railway"
	   "off" "Occupation Rockwall"
	   "off" "Occupation Hedge"
	   "off" "Occupation Other"
	   "off" "Occupations"
	   "off" "Occupation Kerbs"
	   "off" "Occupation Buildings"
	   "off" "Occupation Gate"
	   "off" "Occupation Not Fenced" ""
	   )

  
  
 
  
  
  
		    (princ "\nSelect Easements:")
		    (SETQ lots (SSGET  '((0 . "LWPOLYLINE"))))

		   (command "-layer" "on" "Drafting"
	 	   "on" "Easement"
			    "on" "Boundary"
			    "on" "Boundary Extinguished"
	   "on" "Drafting AFR"
	   "on" "Traverse"
	   "on" "Sideshot"
	   "on" "Topo"
	   "on" "Occupation Fences"
	   "on" "Occupation Walls"
	   "on" "Occupation Building Return"
	   "on" "Occupation Timber Walls"
	   "on" "Occupation Fences"
	   "on" "Occupation Centreline"
	   "on" "Occupation Railway"
	   "on" "Occupation Rockwall"
	   "on" "Occupation Hedge"
	   "on" "Occupation Other"
	   "on" "Occupations"
	   "on" "Occupation Kerbs"
	   "on" "Occupation Buildings"
	   "on" "Occupation Gate"
	   "on" "Occupation Not Fenced" ""
	   )

		     (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots 0)))))

		     (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Selected Easement Geometry with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))

		         (if (/= (setq stringpos (vl-string-search "name" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq pclname (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq pclname ""))

		    (setq ename pclname)
      (setq enamelist (append (list pclname) enamelist))
      
		    
		    (setq count 1)
		    (REPEAT (- (SSLENGTH lots) 1)
		       (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))

     (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Selected Easement Geometry with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))

		         (if (/= (setq stringpos (vl-string-search "name" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq pclname (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq pclname ""))

		      (if (= (member pclname enamelist) nil)
			(progn
			  (setq ename (strcat ename "," pclname ))
			  (setq enamelist (append (list pclname) enamelist))
			  ))
		      (setq count (+ count 1))
		      );repeat
			
		    ));if select polylines
				      
  
 (SETQ pcluse (getstring T (STRCAT "\n Easement Purpose (or * for list)[" ped "]:")))
(if (and (/= ped "")(= pcluse ""))(setq pcluse ped))
  
		    
		     (if (= pcluse"*")
    (progn
		      (setq workingselnum "1")
		      (setq names easepurposelist)
		      (dbox)
		      (setq pcluse returntype)
      (if (= (substr pcluse 1 5) "Group")(setq pcluse (substr pcluse 8 )))
		       ));p&if format*
		    (SETQ ped pcluse)
		    
  
    (setq lotorigin (getstring T "\nOrigin (This Plan):" ))
  (if (or (= lotorigin "")(= lotorigin "This plan")(= lotorigin "This Plan")(= lotorigin "THIS PLAN"))(setq lotstate "created"
													lotorigin "This Plan")(setq lotstate "existing"))
    (setq owner (getstring T (STRCAT "\nBenefit/In Favor of:[" prevowner "]" )))
	  (if (= owner "")(setq owner prevowner))
	   (setq prevowner owner)
	  
   (setq width (getstring T (STRCAT "\nWidth(default is none):" )))
	  



  ;get admin sheet if exist and store elp point
(IF  (/= (setq adminsheet (ssget "_X" '((0 . "INSERT") (2 . "ADMINSHEET")))) nil)(progn
		(SETQ p1 (CDR(ASSOC 10 (ENTGET (SSNAME ADMINSHEET 0)))))
		(SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME ADMINSHEET 0)))))
		(IF (= ELP NIL) (setq elp (list (+ (CAR P1) 14) (+ (CADR P1) 108))));setup postion for easement legend

			(setq count 1)
  (SETQ ATTLIST (LIST))
	    
(setq Obj (vlax-ename->vla-object En))
	(foreach x (vlax-invoke Obj 'GetAttributes)
	  (SETQ ATT (vla-get-textstring X))
	  (if (= att "none") (setq att ""))

	  (setq &pos 0)
	   (while (/=  (setq &pos (vl-string-search "&" att &pos )) nil) (setq att (vl-string-subst "&amp;" "&"  att &pos)
										      &pos (+ &pos 4)))
	 	  	  
	  (setq crlfpos 0)
	   (while (/=  (setq crlfpos (vl-string-search "\\P" att crlfpos )) nil) (setq att (vl-string-subst "&#xA;" "\\P"  att crlfpos)
										      crlfpos (+ crlfpos 5)))


	  (setq attlist (append attlist (list att)))
(setq planno (nth 0 attlist ))
	  )
)
  )
  
  

  (if (= lotstate "created")(progn
			      (if (= planno nil)(setq planno (getstring "\nProposed Plan Number:")))
			      (setq easnumber (getstring "\nEasement Number for xml:"))
    (IF (/= (substr easnumber 1 3) "EAS" )(setq pclname (strcat "EAS" easnumber "\\" planno))(setq pclname (strcat easnumber "\\" planno)));make new name)
			      
			      )
    (setq pclname lotorigin);use entered name
    );if created
  (if (= elp nil)(setq elp (getpoint "\nSelect position for easement table:")))
  
      (setvar "CLAYER" "lot definitions")
						    (command "point" elp)
						    
						  ;(if (/= pcldesc "")(setq pcldescs (strcat " desc=\"" pcldesc "\""))(setq pcldescs ""))
						  ;(if (/= pclarea "")(setq areas (strcat " area=\"" pclarea "\""))(setq areas ""))
						  (if (/= pcluse "")(setq pcluses (strcat " useOfParcel=\"" pcluse "\""))(setq pcluses ""))
						  (if (/= owner "")(setq owners (strcat " owner=\"" owner "\""))(setq owners ""))
 						  (if (vl-string-search "," ename) (setq pcltype "Multipart")(setq pcltype "Single")) 
  
						   ; (if (/= pclformat "")(setq pclformats (strcat " parcelFormat=\"" pclformat "\""))(setq pclformats ""))
						  (SETQ LTINFO (STRCAT "  <Parcel name=\"" pclname "\""  " class=\"Easement\" state=\"" lotstate "\" parcelType=\"" pcltype "\" parcelFormat=\"Standard\"" pcluses owners ">!" ename))
  

                                                       (if (> (strlen ltinfo) 255) (progn
						  (alert (strcat "\nEasement " pclname " purpose too long for xdata 255 character limit, please cosider revising"))
								   (setq pcluses (strcat " useOfParcel=\"" (substr pcluse 1 (- (strlen pcluse) (- (strlen ltinfo) 255)))"\""))
							(SETQ LTINFO (STRCAT "  <Parcel name=\"" pclname "\"" pcldescs  " class=\"" pclclass "\" state=\"" pclstate "\" parcelType=\"" pcltype "\""  pclformats  areas pcluses pclowners ">!" pclref))
										  ))

  (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)

						    (setvar "CLAYER" "Drafting")

						    (COMMAND "text" elp "2.5" "90" ename "")
						    (COMMAND "TEXT" (LIST (+ (CAR ELP) 27)(CADR ELP)) "2.5" "90" pcluse "")
						    (COMMAND "TEXT" (LIST (+ (CAR ELP) 120)(CADR ELP)) "2.5" "90" lotorigin "")
						    (COMMAND "TEXT" (LIST (+ (CAR ELP) 160)(CADR ELP)) "2.5" "90" owner "")
  (if (/= width "")(progn
 (setvar "CLAYER" "Admin Sheet")
 
		       (while (/= (setq ,pos (vl-string-position 44 ename 1)) nil)
    (progn
    
    (setq subparcel (substr ename 1  ,pos ))
    (setq ename (substr ename (+ ,pos 2)))
  
                                                    (COMMAND "MTEXT" (LIST (+ (CAR ELP) 95)(CADR ELP))(LIST (+ (CAR ELP) 18)(+(CADR ELP) 2.5)) WIDTH "")

  	(setq xdatai (strcat "<Annotation type=\"Easement Width\" pclRef=\"" subparcel "\""))
	 (SETQ EN (ENTLAST))
    (SETQ SENTLIST (ENTGET EN))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 xdatai)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

  
    ))

     ;last or only parcel
 
   (setq subparcel ename)
  
                                                    (COMMAND "MTEXT" (LIST (+ (CAR ELP) 95)(CADR ELP))(LIST (+ (CAR ELP) 111.5)(+(CADR ELP) 2.5)) WIDTH "")

  	(setq xdatai (strcat "<Annotation type=\"Easement Width\" pclRef=\"" subparcel "\""))
	 (SETQ EN (ENTLAST))
    (SETQ SENTLIST (ENTGET EN))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 xdatai)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

));if width exists
						    (setq elp (list (car elp)(- (cadr elp) 4)))

  



  
  (setvar "pickstyle" pickstyle)
  )



;-----------------------------------------------------------------------------Create Restriction Link point----------------------------------------------------
(defun C:XLRE (/)
(setq pickstyle (getvar "Pickstyle"))
  (setvar "pickstyle" 0)

  (setq rbenlist (list))
  (setq rburlist (list))
	
  (setvar "CLAYER" "lot definitions")
  
 (setq resname (getstring "Restriction Identifier(or enter to Select polygon):" T))
  (if (= resname "")(progn
      (command
	"-layer" "off" "Drafting"
	       "off" "Boundary"
	"off" "Boundary Extinguished"
	   "off" "Drafting AFR"
	   "off" "Easement"
	   "off" "Traverse"
	   "off" "Sideshot"
	   "off" "Topo"
	   "off" "Occupation Fences"
	   "Off" "Occupation Walls"
	   "off" "Occupation Building Return"
	   "off" "Occupation Timber Walls"
	   "off" "Occupation Fences"
	   "off" "Occupation Centreline"
	   "off" "Occupation Railway"
	   "off" "Occupation Rockwall"
	   "off" "Occupation Hedge"
	   "off" "Occupation Other"
	   "off" "Occupations"
	   "off" "Occupation Kerbs"
	   "off" "Occupation Buildings"
	   "off" "Occupation Gate"
	   "off" "Occupation Not Fenced" ""
	   )

  
		    
		    (SETQ en (car (entsel "\nSelect Restriction:" )))

		   (command "-layer" "on" "Drafting"
	 	   "on" "Easement"
			    "on" "Boundary"
			    "on" "Boundary Extinguished"
	   "on" "Drafting AFR"
	   "on" "Traverse"
	   "on" "Sideshot"
	   "on" "Topo"
	   "on" "Occupation Fences"
	   "on" "Occupation Walls"
	   "on" "Occupation Building Return"
	   "on" "Occupation Timber Walls"
	   "on" "Occupation Fences"
	   "on" "Occupation Centreline"
	   "on" "Occupation Railway"
	   "on" "Occupation Rockwall"
	   "on" "Occupation Hedge"
	   "on" "Occupation Other"
	   "on" "Occupations"
	   "on" "Occupation Kerbs"
	   "on" "Occupation Buildings"
	   "on" "Occupation Gate"
	   "on" "Occupation Not Fenced" ""
	   )

		     
		     (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Selected Restriction Geometry with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))

		         (if (/= (setq stringpos (vl-string-search "name" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq resname (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq pclname ""))

		   
		    
		   		    
		    ));if select polyline
				      
  ;GET ALL LOTS AND LOOK FOR BURDEN BENIFET


  (SETQ BEN (getstring "Land to Benefit (comma delimited):"))
  (setq bur (getstring "Land to Burden (comma delimited):"))

  (setq bens ben
	burs bur);get output strings

    (while (/= (setq ,pos (vl-string-position 44 ben 1)) nil)
    (progn
    
    (setq rbenlist (append rbenlist (list (substr ben 1  ,pos ))))
    (setq ben (substr ben (+ ,pos 2)))
    ))
  (setq rbenlist (append rbenlist (list ben)))

    (while (/= (setq ,pos (vl-string-position 44 bur 1)) nil)
    (progn
    
    (setq rburlist (append rburlist (list (substr bur 1  ,pos ))))
    (setq bur (substr bur (+ ,pos 2)))
    ))
  (setq rburlist (append rburlist (list bur)))

      
  (setq bdyline (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Lot Definitions"))))

      (setq count 0)
      (setq rescount 0)


  (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nObject has not xdata " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
    

 (if (/= (setq stringpos (vl-string-search "name" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq pclname (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq pclname ""))

    ;if in burden list add title
    (if (member pclname rburlist)
      (progn
   
 
  (setq stringpossearch 1)
  (while (/= nil (setq stringpossearch (vl-string-search ">" xdatai (+ stringpossearch 1))))
    (setq stringpos stringpossearch)
    )
    
  (setq stringpos1 (+ stringpos 1)
	stringpos2 (+ stringpos 2) 
	)
  
    
(setq xdatafront (substr xdatai 1  stringpos1 ))
    (setq xdataback (substr xdatai stringpos2 ))
  (if (= (vl-string-search (strcat "<Title name=\"" resname "\" titleType\=\"Restriction Burden\" />") xdatai) nil)
    (progn
    (setq xdatai (strcat xdatafront "<Title name=\"" resname "\" titleType\=\"Restriction Burden\" />" xdataback))

    (SETQ SENTLIST (ENTGET EN))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 xdatai)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
    ));if not already assigned
));if burlist

    ;if in benifet list add title
    (if (member pclname rbenlist)
      (progn
   
 
  (setq stringpossearch 1)
  (while (/= nil (setq stringpossearch (vl-string-search ">" xdatai (+ stringpossearch 1))))
    (setq stringpos stringpossearch)
    )
    
  (setq stringpos1 (+ stringpos 1)
	stringpos2 (+ stringpos 2) 
	)
  
    
(setq xdatafront (substr xdatai 1  stringpos1 ))
    (setq xdataback (substr xdatai stringpos2 ))
  (if (= (vl-string-search (strcat "<Title name=\"" resname "\" titleType\=\"Restriction Benefit\" />") xdatai) nil)
    (progn
    (setq xdatai (strcat xdatafront "<Title name=\"" resname "\" titleType\=\"Restriction Benefit\" />" xdataback))

    (SETQ SENTLIST (ENTGET EN))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 xdatai)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
    ));if not already assigned
));if burlist

  (IF (= pclname resname)(setq rescount (+ rescount 1)));count number of reserves for multi or single

    (setq count (+ count 1))


);repeat

  
  	       
(IF (= RLP nil)(progn
(IF  (/= (setq adminsheet (ssget "_X" '((0 . "INSERT") (2 . "ADMINSHEET")))) nil)(progn
		(SETQ p1 (CDR(ASSOC 10 (ENTGET (SSNAME ADMINSHEET 0)))))
		(SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME ADMINSHEET 0)))))
		(setq RLP (list (+ (CAR P1) 298) (+ (CADR P1) -45)));setup postion for easement legend
))
))
  
(IF (= RLP NIL)(SETQ RLP (GETPOINT "Restriction Description Position:")))

  

  
      (setvar "CLAYER" "lot definitions")
						    (command "point" rlp)
						    
						  ;(if (/= pcldesc "")(setq pcldescs (strcat " desc=\"" pcldesc "\""))(setq pcldescs ""))
						  ;(if (/= pclarea "")(setq areas (strcat " area=\"" pclarea "\""))(setq areas ""))
						  ;(if (/= pcluse "")(setq pcluses (strcat " useOfParcel=\"" pcluse "\""))(setq pcluses ""))
						  ;(if (/= pclowner "")(setq owners (strcat " owner=\"" owner "\""))(setq owners ""))
                                                  (if (> rescount 1)(setq parceltypes "Multipart")(setq parceltypes "Single"))
						   ; (if (/= pclformat "")(setq pclformats (strcat " parcelFormat=\"" pclformat "\""))(setq pclformats ""))
						  (SETQ LTINFO (STRCAT "  <Parcel name=\"" resname "\""  " class=\"Restriction\" state=\"created\" parcelType=\"" parceltypes "\">" ))
						    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)
  


  (if (vl-string-position 44 bens)(setq bens (strcat "s " bens))(setq bens (strcat " " bens)))
  (if (vl-string-position 44 burs)(setq burs (strcat "s " burs))(setq burs (strcat " " burs)))
										(setvar "CLAYER" "Admin Sheet")
																				     
										( command "text" rlp "3.5" "90" (strcat "Creation of Restriction " resname) )
										(setq rlp (list (car rlp)(- (cadr rlp) 5)))
										( command "text" rlp "2.5" "90" (strcat "Land to Benefit: Lot" bens ) )
										(setq rlp (list (car rlp)(- (cadr rlp) 4)))
										( command "text" rlp "2.5" "90" (strcat "Land to Burden: Lot" burs ) )
										(setq rlp (list (car rlp)(- (cadr rlp) 4)))

										
			(setq pcldesc (getstring  T "Restriction Description:"))

(if (/= pcldesc "")(progn
										
										(command "mtext" rlp "h" "2.5" "@250,-10" pcldesc "")
										 (SETQ LTINFO2 resname )
						    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO2)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)
										(setq rlp (list (car rlp) (- (cadr rlp) (+ 10(* 4  (/ (strlen pcldesc) 100))))))
		     ))
										
									









  
  (setvar "pickstyle" pickstyle)
  )

;------------------------------------------------------Create Road Lot----------------------------------------------

(defun C:XCR (/)
(setq pickstyle (getvar "Pickstyle"))
  (setvar "pickstyle" 0)

  (setq prevlayer (getvar "CLAYER"))

  (SETVAR "CLAYER"  "Lot Definitions" )
  (setq lotc (getpoint "\nSelect Lot Centre:"))
  
(command "-layer" "off" "Drafting"
	   "off" "Drafting AFR"
	   "off" "Easement"
	   "off" "Traverse"
	   "off" "Sideshot"
	   
	   "off" "Occupation Fences"
	   "Off" "Occupation Walls"
	   "off" "Occupation Building Return"
	   "off" "Occupation Timber Walls"
	   "off" "Occupation Fences"
	   "off" "Occupation Centreline"
	   "off" "Occupation Railway"
	   "off" "Occupation Rockwall"
	   "off" "Occupation Hedge"
	   "off" "Occupation Other"
	   "off" "Occupations"
	   "off" "Occupation Kerbs"
	   "off" "Occupation Buildings"
	   "off" "Occupation Gate"
	   "off" "Occupation Not Fenced" ""
	   )

  
  (command "-hatch" "A" "I" "N" "N" "" "p" "s" lotc "")
  (command "-layer" "on" "Drafting"
	 	   "on" "Easement"
	   "on" "Drafting AFR"
	   "on" "Traverse"
	   "on" "Sideshot"
	   "on" "Topo"
	   "on" "Occupation Fences"
	   "on" "Occupation Walls"
	   "on" "Occupation Building Return"
	   "on" "Occupation Timber Walls"
	   "on" "Occupation Fences"
	   "on" "Occupation Centreline"
	   "on" "Occupation Railway"
	   "on" "Occupation Rockwall"
	   "on" "Occupation Hedge"
	   "on" "Occupation Other"
	   "on" "Occupations"
	   "on" "Occupation Kerbs"
	   "on" "Occupation Buildings"
	   "on" "Occupation Gate"
	   "on" "Occupation Not Fenced" ""
	   )
  (SETQ lothatch (ENTLAST))
  (SETQ ENTSS (SSADD))
  (SSADD lothatch ENTSS)
(command "._HATCHGENERATEBOUNDARY" ENTSS "");edited for Bricscad, much neater, courtesy Jason Bourhill www.cadconcepts.co.nz
  (COMMAND "SELECT" "" "")
  (COMMAND "ERASE" ENTSS "")
(SETQ lotedge (ENTLAST))
  (SETQ ENTSS (SSADD))
  (SSADD lotedge ENTSS)
  
  
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))

  (SETQ PTLIST (LIST))
				
;CREATE LIST OF POLYLINE POINTS
    (SETQ PTLIST (LIST))
	    (foreach a SENTLIST
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	     
	    )				;FOREACH 			)


  (setq ptlist (append ptlist (list (nth 0 ptlist))))


 (SETQ lotno (getstring T"\n Road/Reserve Identifier (eg R#/RES#):"))
  (SETQ desc (getstring T"\n Road Name:"))
  (SETQ pclowner (getstring T (STRCAT "\n Council/Body/Person [" prevowner "]:")))
  (if (= pclowner "")(setq pclowner prevowner))
  (setq prevowner pclowner)
  

  (setq area (getstring "\nArea or [C]alculate (mm.dm) (aa.rr.pp.f/p) [Last]:"))
(if (or (= area "")(= area "l")(= area "L")(= area "LAST")(= area "last"))(setq area "Last"))

  (if (= area "Last" )(setq area arealast))
  (SETQ arealast area)


  ;deal with imperial areas
  
      
	(setq dotpos1 (vl-string-position 46 area 0))
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 area (+ dotpos1 1))))
	(if (/= dotpos2 nil)(progn;idenfited as imperial area, must have second dotpos to work
			      
	(if (= dotpos2 nil)(setq dotpos3 nil)(setq dotpos3 (vl-string-position 46 area (+ dotpos2 1))))
	(setq /pos1 (vl-string-position 47 area 0))
	(if (/= /pos1 nil);with factional part
	  (progn
	    (setq den (substr area ( + /pos1 2) 50))
	    (setq num (substr area ( + dotpos3 2) (- (- /pos1 dotpos3) 1)))
	    (setq fperch (/ (atof num) (atof den)))
	    (setq perch (substr area (+ dotpos2 2) (- (- dotpos3 dotpos2) 1)))
	    (setq perch (+ fperch (atof perch)))
	    (setq rood (substr area (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq perch (+ perch (* (atof rood) 40)))
	    (setq acre (substr area 1  dotpos1 ))
	    (setq perch (+ perch (* (atof acre) 160)))
	    (setq area (rtos (* perch 25.2929538117) 2 9))
	    )
	  )
	(if (and (/= dotpos1 nil)(/= dotpos2 nil)(= /pos1 nil));without fractional part
	  (progn
	    (setq perch (substr area ( + dotpos2 2) 50))
	    (setq perch (atof perch))
	    (setq rood (substr area (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq perch (+ perch (* (atof rood) 40)))
	    (setq acre (substr area 1  dotpos1 ))
	    (setq perch (+ perch (* (atof acre) 160)))
	    (setq area (rtos (* perch 25.2929538117) 2 9))
	    )
	  )
	
	));p&if imperial area
	  

  
   
   (SETQ area1 (vlax-get-property (vlax-ename->vla-object sent ) 'area ))

  ;(setvar "dimzin" 0)
  (IF (or ( = area "C")(= area "c"))
    (progn
      (setq area (rtos area1 2 3))
      (setq area1 (atof (rtos area1 2 3)));deal with recurring 9's
      					    (if (> area1 0)(setq textarea (strcat (rtos (*  (/ area1 0.1) 0.1) 2 1) "m²")))
					    (if (> area1 100)(setq textarea (strcat (rtos (*  (/ area1 1) 1) 2 0) "m²")))
      					    (if (> area1 10000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 0.001) 0.001) 2 3) "ha")))
					    (if (> area1 100000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 0.01) 0.01) 2 2) "ha")))
					    (if (> area1 1000000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 0.1) 0.1) 2 1) "ha")))
					    (if (> area1 10000000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 1) 1) 2 0) "ha")))
                                            (if (> area1 100000000) (setq textarea (strcat (rtos (*  (/ (/ area1 1000000) 0.1) 0.1) 2 1) "km²")))
                                            (if (> area1 1000000000) (setq textarea (strcat (rtos (*  (/ (/ area1 1000000) 1) 1) 2 0) "km²")))
      
      
					    
      )
    (progn
     (setq areapercent (ABS(* (/  (- area1 (ATOF area)) area1) 100)))
     (if (> areapercent 10) (alert (strcat "\nArea different to calulated by " (rtos areapercent 2 0)"%")))

      (if (> (atof area) 1)(setq textarea (strcat (rtos (atof area) 2 3) "m²")))
 					    (if (> (atof area) 0)(setq textarea (strcat (rtos (* (/ (atof area) 0.1) 0.1) 2 1) "m²")))
					    (if (> (atof area) 100)(setq textarea (strcat (rtos (* (/ (atof area) 1) 1) 2 0) "m²")))
      					    (if (> (atof area) 10000) (setq textarea (strcat (rtos (* (/ (/ (atof area) 10000) 0.001) 0.001) 2 3) "ha")))
					    (if (> (atof area) 100000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 0.01) 0.01) 2 2) "ha")))
					    (if (> (atof area) 1000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 0.1) 0.1) 2 1) "ha")))
					    (if (> (atof area) 10000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 1) 1) 2 0) "ha")))
                                            (if (> (atof area) 100000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 1000000) 0.1) 0.1) 2 1) "km²")))
                                            (if (> (atof area) 1000000000) (setq textarea (strcat (rtos (* (/ (/ (atof area) 1000000) 1) 1) 2 0) "km²")))

     )
    )



  
  
  
   
  (setq lotc (trans lotc 1 0));convert to world if using UCS
  (if (= (substr lotno 1 3) "RES")(setq pclclass "Reserve")(setq pclclass "Road"))
  (if (/= desc "")(setq odesc (strcat " desc=\"" desc "\""))(setq odesc ""))
  (SETQ LTINFO (STRCAT "<Parcel name=\"" lotno "\"" odesc " area=\"" area "\" class=\"" pclclass "\" state=\"created\" parcelType=\"Single\" parcelFormat=\"Standard\" owner=\"" pclowner "\">!"
		       (rtos (cadr lotc) 2 6 ) " " (rtos (car lotc) 2 6)))
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

  (setq THF 2)

         (setq lotc (trans lotc 0 1));convert back to UCS if using one

 
  (setq rpos (polar lotc (* 1.5 pi) (* th 2.5)))
  (setq areapos (polar lotc (* 1.5 pi) (* th 5)))
  (if (/= desc "ROAD")(PROGN (SETVAR "CELWEIGHT" 50)
			(SETQ RTH (* TH 2)))
    (SETQ RTH (* TH 1.4)))
  (SETVAR "CLAYER"  "Drafting" )
  (if (/= "" desc)
    (progn
  (COMMAND "TEXT" "J" "BC" lotc RTH "90" desc)
  (setq roadname (entget (entlast)))
  ))
  (SETVAR "CELWEIGHT" 35)
  (COMMAND "TEXT" "J" "BC" areapos (* TH 1.4) "90"  textarea )
  (COMMAND "TEXT" "J" "BC" rpos (* TH 1.4) "90"  lotno )
(SETVAR "CELWEIGHT" -1)
 


(setq lotc (trans lotc 1 0));convert to world if using UCS

  (if (/= desc "")
    (progn
		(setq count 0)
		(setq minoff 100000000000000000000000000000000000000)
(repeat (- (length ptlist)2 )

  (setq op1 (nth count ptlist))
  (setq op2 (nth (+ count 1) ptlist))
  (setq op3 (nth (+ count 2) ptlist))



  ;check line one
;check offset to line
  (SETQ ANG (ANGLE oP1 oP2))
  (SETQ CANG (+ ANG (/ PI 2)))
  (SETQ P4 (POLAR lotc CANG 1000))
  (SETQ P6 (POLAR lotc (+ CANG PI) 2000))
  (IF (SETQ P5 (INTERS oP1 oP2 P6 P4 ))(PROGN

					 (SETQ OFF (DISTANCE lotc P5))

    (if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang cang)
      );if
    
  ));p & if inters

    ;check inside deflection angle
    (setq ang2 (angle op2 op3))
    (setq cang2 (+ ang (/ pi 2)))
    (setq ltopang (angle op2 lotc))
    (setq defl (- ang2 ang))
(if (/= defl 0);check for straight line
  (progn
    (if (< defl pi)(progn
		     (setq ray1 (- ang (/ pi 2)))
		     ;(if (< ray1 0) (setq ray1 (+ ray1 (* 2 pi))))
		     (setq ray2 (- ang2 (/ pi 2)))
		     ;(if (< ray2 0) (setq ray2 (+ ray2 (* 2pi))))
		     ));p and if less than pi
    (if (> defl pi)(progn
		     (setq ray1 (+ ang (/ pi 2)))
		     ;(if (> ray1 (* 2 pi)(setq ray1 (- ray1 (* 2 pi)))))
		     (setq ray2 (+ ang (/ pi 2)))
		     ;(if (> ray2 (* 2 pi)(setq ray2 (- ray2 (* 2 pi)))))
		     ));p and if greater than pi

    (if (or (and (> ltopang ray1) (< ltopang ray2))(and (> ltopang ray2)(< ltopang ray1)));check ot see if inside deflection squares **** needs testing later
      (progn
	(setq off (distance lotc op2))
(if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang ltopang)
      );if
	));p and if in defl sqr
   ));p and if not straight line

  (setq count (+ count 1))
    );repeat
      
				;using this info change the road name text rotation to the angle of the line
		(setq rrot (+ nearang (/ pi 2)))
		(if (> rrot (* 2 pi))(setq rrot (- rrot (* 2 pi))))
		(IF (AND (> rrot  (* 0.5 pi)) (< rrot (* 1.5 pi)))(setq rrot (+ rrot pi))) ;if text is upsidedown reverse rotation
  (setq p1 (polar lotc rrot 10))
  (setq llotc (trans lotc 0 1))
  (setq lp1 (trans p1 0 1))
  (setq oang (angle llotc lp1))
  (IF (AND (> oang  (* 0.5 pi)) (< oang (* 1.5 pi)))(setq rrot (+ rrot pi))) ;if text is upsidedown reverse rotation					  
		
    
  (SETQ	roadname (subst (cons 50  rrot);SUB IN NEW POINT 2 HEIGHT
		     (assoc 50 roadname)
		     roadname     ) )
  (ENTMOD roadname)
  
  	       (SETQ roadname (subst (cons 8  "Drafting AFR")(assoc 8 roadname) roadname))
	       (SETQ roadname (subst (cons 50  rrot )(assoc 50 roadname) roadname))
		(ENTMAKE roadname)
))

   ;get admin sheet if exist and store rlp point
  (IF (= RDLP nil)(progn
(IF  (/= (setq adminsheet (ssget "_X" '((0 . "INSERT") (2 . "ADMINSHEET")))) nil)(progn
		(SETQ p1 (CDR(ASSOC 10 (ENTGET (SSNAME ADMINSHEET 0)))))
		(SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME ADMINSHEET 0)))))
		(setq RDLP (list (+ (CAR P1) 28) (+ (CADR P1) 255)));setup postion for easement legend
))
(IF (= RDLP NIL)(SETQ RDLP (GETPOINT "Road Table Position:")))
))
  
(COMMAND "TEXT" "J" "BL" RDLP 2.5 "90"  LOTNO )
(COMMAND "TEXT" "J" "BL" (LIST (+ (CAR RDLP) 37)(CADR RDLP)) 2.5 "90" PCLOWNER )
  (SETQ RDLP (LIST (CAR RDLP)(- (CADR RDLP) 4)))


  (SETVAR "CLAYER" prevlayer) 
(COMMAND "DRAWORDER" ENTSS "" "BACK")
(setvar "pickstyle" pickstyle)
  )



;---------------------------------------------------------------CREATE OWNERS CORPORATION----------------------------------------------------------

(defun C:XCOC (/)
  (setq ocp (getpoint "\nSelect position for Owners Corporation Table"))

  

  ;get admin sheet if exist and store elp point
(IF  (/= (setq adminsheet (ssget "_X" '((0 . "INSERT") (2 . "ADMINSHEET")))) nil)(progn
		(SETQ p1 (CDR(ASSOC 10 (ENTGET (SSNAME ADMINSHEET 0)))))
		(SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME ADMINSHEET 0)))))
		

			(setq count 1)
  (SETQ ATTLIST (LIST))
	    
(setq Obj (vlax-ename->vla-object En))
	(foreach x (vlax-invoke Obj 'GetAttributes)
	  (SETQ ATT (vla-get-textstring X))
	  (if (= att "none") (setq att ""))

	  (setq &pos 0)
	   (while (/=  (setq &pos (vl-string-search "&" att &pos )) nil) (setq att (vl-string-subst "&amp;" "&"  att &pos)
										      &pos (+ &pos 4)))
	 	  	  
	  (setq crlfpos 0)
	   (while (/=  (setq crlfpos (vl-string-search "\\P" att crlfpos )) nil) (setq att (vl-string-subst "&#xA;" "\\P"  att crlfpos)
										      crlfpos (+ crlfpos 5)))


	  (setq attlist (append attlist (list att)))
(setq planno (nth 0 attlist ))
	  )
)
  )

  (if (= planno nil)(setq planno (getstring "\nProposed Plan Number:")))
(setvar "CLAYER" "lot definitions")
  (command "point" ocp)
  
  (SETQ LTINFO (STRCAT "    <Parcel name=\"OC1\\" planno "\" desc=\"Owners Corporation\" class=\"Owners Corporation\" state=\"created\" parcelType=\"Single\" useOfParcel=\"Unlimited\">"))
						    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)

   (setvar "CLAYER" "Drafting")
						    (COMMAND "text" ocp "2.5" "90" "PARCEL" )
						    (COMMAND "TEXT" (LIST (+ (CAR ocp) 40)(CADR ocp)) "2.5" "90" "ENTITLEMENT" )
						    (COMMAND "TEXT" (LIST (+ (CAR ocp) 70)(CADR ocp)) "2.5" "90" "LIABILITY" )
						    
						    (setq ocp (list (car ocp)(- (cadr ocp) 4)))

  (setq ocpstring nil)
  (while (/= ocpstring "")(progn
			    (if (/= ocpstring "")(progn
  (setq ocpstring (getstring "\nOwners Corp Details (Lot,Entitlement,Liability):"))

  (setq ,pos1 (vl-string-position 44 ocpstring 0))
      (setq ,pos2 (vl-string-position 44 ocpstring (+ ,pos1 1)))
      (setq pclref (substr ocpstring 1 ,pos1))
      (setq lotent (substr ocpstring (+ ,pos1 2) (- (- ,pos2 ,pos1) 1)))
      (setq lotliab (substr ocpstring (+ ,pos2 2) 50))

  (setq pclref (strcat pclref "\\" planno))

(setvar "CLAYER" "lot definitions")
						    (command "point" ocp)
						    
						    (SETQ LTINFO (STRCAT "  pclRef=\"" pclref "\" lotEntitlements=\"" lotent "\" liabilityApportionment=\"" lotliab "\"/>"))
						    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)

						    (setvar "CLAYER" "Drafting")

						    (COMMAND "text" ocp "2.5" "90" pclref "")
						    (COMMAND "TEXT" (LIST (+ (CAR ocp) 50)(CADR ocp)) "2.5" "90" lotent )
						    (COMMAND "TEXT" (LIST (+ (CAR ocp) 75)(CADR ocp)) "2.5" "90" lotliab )

						    (setq ocp (list (car ocp)(- (cadr ocp) 4)))
));p&if
			    ));p&w






	);defun


       

;---------------------------------------------------------------REDEFINE LOT CENTRES---------------------------------------------
(defun C:XMLC (/)

  (setq lots (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Lot Definitions"))))
  (command "layiso" "s" "off" "" lots "")

  (setq lot (car (entsel "\nSelect Refernce lot")))
  (command "layuniso")

  (setq centext (car (entsel "\nSelect \"Lot Number\" Text")))

    (command "layiso" lots "")
  (princ "\nSelect Lots to apply shift to:")
   (setq lotstbe (ssget '((0 . "LWPOLYLINE") (8 . "Lot Definitions"))))
(command "layuniso");change from layerp for BricsCAD
  
     	    (SETQ XDATAI (ENTGET lot '("LANDXML")))
	   (SETQ XDATAI (ASSOC -3 XDATAI))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))

       (setq !pos1 (vl-string-position 33 xdatai 0))
                      (setq lotc (substr xdatai (+ !pos1 2) 200))
                      (setq xdatai  (substr xdatai 1 !pos1))

(setq spcpos1 (vl-string-position 32 lotc 0))
                      (setq eeast (atof(substr lotc (+ spcpos1 2) 200)))
                      (setq enorth  (atof (substr lotc 1 spcpos1)))

 (SETQ P1 (CDR(ASSOC 11 (ENTGET centext))))
  (setq neast (car p1))
  (setq nnorth (cadr p1))
  (setq deltae (- neast eeast))
  (setq deltan (- nnorth enorth))

  (SETQ COUNT 0)
  (repeat (sslength lotstbe)
     (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lotstbe COUNT)))))
    (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME lots COUNT)))))
      	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL)(princ (strcat "\nLot with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3))))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))

      (setq !pos1 (vl-string-position 33 xdatai 0))
                      (setq lotc (substr xdatai (+ !pos1 2) 200))
                      (setq xdatai  (substr xdatai 1 !pos1))

    (setq spcpos1 (vl-string-position 32 lotc 0))
                      (setq east (ATOF(substr lotc (+ spcpos1 2) 200)))
                      (setq north  (ATOF(substr lotc 1 spcpos1)))
    (setq nlotc (strcat "!" (rtos (+ north deltan) 2 3) " " (rtos (+ east deltae) 2 3)))
    (setq LTINFO (strcat xdatai nlotc))

    (SETQ SENT EN)
  (SETQ SENTLIST (ENTGET SENT))

  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
(SETQ COUNT (+ COUNT 1))
    )

  (princ (strcat "\n Shift of " (rtos deltae 2 3) "," (rtos deltan 2 3) " applied to " (rtos count 2 0) " lots"))
  
  );d




;----------------------------------------------------------------NON PEG CORNER MARK--------------------------------------------

(defun C:XCM (/)

    ;GET 1ST LINE INFO
    (graphscr)
    (setq prevlayer (getvar "CLAYER"))
  
    (setq p1 (getpoint "\nSelect Corner with mark: "))


(setq prmtype rmtype)
  (setq rmtype (getstring T (strcat "\nType [" rmtype "](* for list)(#-comment):")))

    ;look for mark comment
  (setq minpos1 (vl-string-position 45 rmtype 0))
	(if (= minpos1 nil)(setq rmcomment "")(progn
					      (setq rmcomment (substr rmtype ( + minpos1 2) 50))
					      (setq rmtype (substr rmtype 1  minpos1 ))
					      )
	  )
  (if (= rmtype "*")(progn
		      (setq workingselnum tselnum)
		      (setq names rmtypelist)
		      (dbox)
		      (setq rmtype returntype)
		      (setq tselnum workingselnum)
		      )
    )
  

    
  (if (= rmtype "")(setq rmtype prmtype ))

   (if (and (= rmcomment "")(= rmtype "Other"))
	   (setq rmcomment (getstring T "\nOther Selected, please give type:"))
  )
  

  (if (= (member rmtype rmtypelist) nil) (progn
					     (Alert "\nType not fount, please select from list" )
					   (setq workingselnum tselnum)
					      (setq names rmtypelist)
		      (dbox)
		      (setq rmtype returntype)
					   (setq workingselnum tselnum)
					     )
    )
  (if (= rmtype "None")(setq rmtype ""));gone or not found mark

 
  
  (setq prmstate rmstate)

  ;(IF (= rmtype "")(progn;deal with rmtype none with selector
;			 (setq rmstate (getstring "\nNone Selected - Gone or Not Found[G/N](default is Gone):"))
;			 (if (or (= rmstate "n") (= rmstate "N")) (setq rmstate "Not Found"))
;			 (if (or (= rmstate "g") (= rmstate "G") (= rmstate "")) (setq rmstate "Gone"))
;			 (setq rmcondition "")
;			 )
    
 ;   (progn;else just do a normal state selection
			       
   (setq rmcondition (getstring (strcat "\nCondition egFound/Placed[f/p](* for list)(default is Found):")))
   (if (= rmcondition "*")(progn
			    (setq workingselnum cselnum)
		      (setq names rmconditionlist)
		      (dbox)
		      (setq rmcondition returntype)
			    (setq cselnum workingselnum)
		      )
    )
  (if (or (= rmcondition "f")(= rmcondition "F")(= rmcondition "")) (setq rmcondition "Found"))
  (if (or (= rmcondition "p")(= rmcondition "P")) (setq rmcondition "Placed"))

  (if (= (member rmcondition rmconditionlist) nil) (progn
					     (Alert "\nCondition not fount, please choose from list" )
					     (setq workingselnum cselnum)
					     (setq names rmconditionlist)
		      (dbox)
		      (setq rmcondition returntype)
					     (setq cselnum workingselnum)
					     )
    )

  (if ( = rmcondition "Found") (setq crmstate "Existing"))
  (if ( = rmcondition "Placed")(setq crmstate "New"))
  
  (setq rmstate (getstring (strcat "\nState eg New/Existing[n/e]("crmstate"):")))
  
    (if (= rmstate "")(setq rmstate crmstate))
(if (or (= rmstate "n") (= rmstate "N")(= rmstate "")) (setq rmstate "New"))
(if (or (= rmstate "e") (= rmstate "E")) (setq rmstate "Existing"))
  
  
  
  ;)
  ;);if type None


  (if (= rmstate "Existing")(progn
   (setq rmrefdp (getstring T(strcat "\nReference plan number (default is nil)[Last-"lrmrefdp"]:")))
   
   (if (or (= rmrefdp "LAST")(= rmrefdp "Last")(= rmrefdp "L")(= rmrefdp "l")(= rmrefdp "last"))(setq rmrefdp lrmrefdp))
   (setq lrmrefdp rmrefdp)
  
    
   )
    (setq rmrefdp "")
    )
   

  


  (SETVAR "CLAYER"  "Monument" )
  (if (= rmtype "Occupation")(setvar "clayer" "Occupations"))
  
  (COMMAND "POINT" P1)

  ;check for no values and replace with "none"
  (if (= rmrefdp "")(setq rmrefdp "none"))
  (if (= rmcondition "")(setq rmcondition "none"))
  (if (= rmrefdp "")(setq rmrefdp "none"))
  (if (/= rmtype "")(setq ormtype (strcat "type=\"" rmtype "\""))(setq ormtype ""))
 
   (if  (/= rmcomment "")(setq ormcomment (strcat " desc=\"" rmcomment "\""))(setq ormcomment ""))
   (if (/= rmcondition "none")(setq ormcondition (strcat " condition=\"" rmcondition "\""))(setq ormcondition ""))
   (if (/= rmrefdp "none")(setq ormrefdp (strcat " originSurvey=\"" rmrefdp "\""))(setq ormrefdp ""))

  
    (SETQ PTINFO (STRCAT  ormtype " state=\""  rmstate "\"" ormcondition  ormrefdp  ormcomment " />" ));Note comment for desc in xml added to distance entry seperated by a space
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 PTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

  

    (lcm)
 


  (setvar "clayer" prevlayer )
)





;----------------------------------------------------------------CREATE PM--------------------------------------------

(defun C:XPM (/)
   (setq prevlayer (getvar "CLAYER"))

  (setq pmpos (getpoint "\nSelect PM/PCM position:"))
  (setq pminfo (getstring t "\nPM information in Format Number,NineFigure,Easting,Northing,Order,Date,Technique(GPS/Cadastral),Source(OSG,Firm),datum(MGA_ZoneXX)\n,Type,Condition,State(Existing/New):" ))
  


		 
      (setq ,pos1 (vl-string-position 44 pminfo 0))
      (setq ,pos2 (vl-string-position 44 pminfo (+ ,pos1 1)))
      (setq ,pos3 (vl-string-position 44 pminfo (+ ,pos2 1)))
      (setq ,pos4 (vl-string-position 44 pminfo (+ ,pos3 1)))
      (setq ,pos5 (vl-string-position 44 pminfo (+ ,pos4 1)))
      (setq ,pos6 (vl-string-position 44 pminfo (+ ,pos5 1)))
      (setq ,pos7 (vl-string-position 44 pminfo (+ ,pos6 1)))
      (setq ,pos8 (vl-string-position 44 pminfo (+ ,pos7 1)))
      (setq ,pos9 (vl-string-position 44 pminfo (+ ,pos8 1)))
      (setq ,pos10 (vl-string-position 44 pminfo (+ ,pos9 1)))
      (setq ,pos11 (vl-string-position 44 pminfo (+ ,pos10 1)))
      

		 
      (setq pmnum (substr pminfo 1 ,pos1))
      (setq pmnf (substr pminfo (+ ,pos1 2) (- (- ,pos2 ,pos1) 1)))
      (setq pmeast (substr pminfo (+ ,pos2 2) (- (- ,pos3 ,pos2) 1)))
      (setq pmnorth (substr pminfo (+ ,pos3 2) (- (- ,pos4 ,pos3) 1)))
      (setq pmorder (substr pminfo (+ ,pos4 2) (- (- ,pos5 ,pos4) 1)))
      (setq pmdate (substr pminfo (+ ,pos5 2) (- (- ,pos6 ,pos5) 1)))
      (setq pmtech (substr pminfo (+ ,pos6 2) (- (- ,pos7 ,pos6) 1)))
      (setq pmsource (substr pminfo (+ ,pos7 2) (- (- ,pos8 ,pos7) 1)))
      (setq pmdatum (substr pminfo (+ ,pos8 2) (- (- ,pos9 ,pos8) 1)))
      (setq rmtype (substr pminfo (+ ,pos9 2) (- (- ,pos10 ,pos9) 1)))
      (setq rmcondition (substr pminfo (+ ,pos10 2) (- (- ,pos11 ,pos10) 1)))
      (setq rmstate (substr pminfo (+ ,pos11 2) 50))
  

  (setq pmdateo pmdate)

  ;sort date entrys

  ;replace /,\,. with -
  (setq /pos 0)
	      (while (/=  (setq /pos (vl-string-search "/" pmdate /pos )) nil) (setq pmdate (vl-string-subst "-" "/"  pmdate /pos)
										      /pos (+ /pos 1)))
(setq /pos 0)
	      (while (/=  (setq /pos (vl-string-search "\\" pmdate /pos )) nil) (setq pmdate (vl-string-subst "-" "\\"  pmdate /pos)
										      /pos (+ /pos 1)))
  (setq /pos 0)
	      (while (/=  (setq /pos (vl-string-search "." pmdate /pos )) nil) (setq pmdate (vl-string-subst "-" "."  pmdate /pos)
										      /pos (+ /pos 1)))
  (setq pmdateo pmdate)
  
  (setq minuspos1 (vl-string-position 45 pmdate 0))
  (setq minuspos2 (vl-string-position 45 pmdate (+ minuspos1 1)))
  (if (or (= minuspos1 1)(= minuspos1 2))(progn;rearrage date
				       (setq day  (substr pmdate 1 minuspos1))
				       (if (= (strlen day) 1) (setq day (strcat "0" day)));single digit days
				       (setq month (substr pmdate (+ minuspos1 2) (- (- minuspos2 minuspos1) 1)))
				       (if (= (strlen month) 1) (setq month (strcat "0" month)));single digit days
				       (setq year  (substr pmdate (+ minuspos2 2) 50))
				       (setq pmdate (strcat year "-" month "-" day))
				       ));p&if dos round the wrong way

		 (SETVAR "CLAYER"  "PM" )
		 
     (COMMAND "POINT" pmpos)

  
 
  

  
  
		 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  ;state added to end of xml
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 (strcat "desc=\"" pmnum
							      "\" oID=\"" pmnf
							      "\" latitude=\"" pmnorth
							      "\" longitude=\"" pmeast
							      "\"  order=\"" pmorder
							      "\" horizontalFix=\"" pmtech
							      "\" horizontalAdjustment=\"" pmsource
							      "\" horizontalDatum=\"" pmdatum
							      "\" currencyDate=\"" pmdate "\"/>" ))))));@@@@change to xml format
  (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)


		 (SETVAR "CLAYER"  "Drafting AFR" )
  
 (IF (= (SUBSTR PMNUM 1 3) "PCM")
     (COMMAND "._INSERT" "VPCM" "_S" TH pmpos "0");DRAW PCM BLOCK
     (COMMAND "._INSERT" "VPM" "_S" TH pmpos "0");ELSE PM BLOCK
     )
  
  (SETQ RMB (ENTLAST))
  (SETQ ENTSS (SSADD))
  (SSADD RMB ENTSS)
(COMMAND "DRAWORDER" ENTSS "" "FRONT")

  (IF (= (SUBSTR PMNUM 1 3) "PCM")
    (PROGN
      (SETQ AANG (/ pi 2))
(SETQ 3POS (POLAR PMPOS (- AANG (* 0.5 PI)) (* TH 3)))
      (COMMAND "TEXT" "J" "BL"  3POS  TH  "45" PMNUM)
      )
    (PROGN
      

(SETQ TEXTPOS (LIST (+ (CAR PMPOS) TH) (+ (CADR PMPOS) (* 0.5 TH))))
  ;NSW(IF (= pmstate "Found")  (SETQ PMNUMS (STRCAT PMNUM " FD")))
  (IF (= rmstate "Placed")  (SETQ PMNUMS (STRCAT PMNUM " PL"))(SETQ PMNUMS PMNUM))
		 (COMMAND "TEXT" "J" "BL"  TEXTPOS (* TH 1.4) "90" PMNUMS)
  (SETQ TEXTPOS (LIST (+ (CAR PMPOS) TH) (+ (CADR PMPOS) (* -1.25 TH))))
  ;NSW(IF (and (/= pmclass "U") (= pmsource "SCIMS" ))(COMMAND "TEXT" "J" "BL"  TEXTPOS (* TH 1.4) "90" "(EST)"))
  ))




  

  (if (= pmboxmark nil)(progn


			 ;get metadata from admin sheet if it exsits
			 



			 
			 (setq pmboxmark (getpoint "\nSelect Point for PM notation box:"))

			 (setq p10 (list (+ (car pmboxmark) (* 96 th))(+ (cadr pmboxmark) (* -2.5 th))))
			 (setq p11 (list (+ (car pmboxmark) 0)(+ (cadr pmboxmark) (* -2.5 th))))
			 (setq p12 (list (+ (car pmboxmark) (* th 48))(+ (cadr pmboxmark) (* -1.25 th))))
			 (command "rectangle" pmboxmark p10)
			 (command "text" "j" "mc" p12 th "90" "SCHEDULE OF COORDINATES USED")
			 (setq pmboxmark p11)

			 (setq p10 (list (+ (car pmboxmark) (* 96 th))(+ (cadr pmboxmark) (* -2.5 th))))
			 (setq p11 (list (+ (car pmboxmark) 0)(+ (cadr pmboxmark) (* -2.5 th))))
			 (setq p122 (list (+ (car pmboxmark) (* th 48))(+ (cadr pmboxmark) (* -1.25 th))))
			 (command "rectangle" pmboxmark p10)
			 ;removed coordinate info box and put at end
			 (setq pmboxmark p11)
			 ;box corners
			 (setq p10 (list (+ (car pmboxmark) 0)(+ (cadr pmboxmark) (* -2.5 th))));NUM
			 (setq p11 (list (+ (car pmboxmark) (* 22 th))(+ (cadr pmboxmark)  0 )));EAST
			 (setq p12 (list (+ (car pmboxmark) (* 35 th))(+ (cadr pmboxmark) (* -2.5 th))));NORTH
			 (setq p13 (list (+ (car pmboxmark) (* 49 th))(+ (cadr pmboxmark)  0 )));HEIGHT
			 (setq p14 (list (+ (car pmboxmark) (* 55 th))(+ (cadr pmboxmark) (* -2.5 th))));ORDER
			 (setq p15 (list (+ (car pmboxmark) (* 61 th))(+ (cadr pmboxmark)  0 )));SOURCE
			 (setq p16 (list (+ (car pmboxmark) (* 74 th))(+ (cadr pmboxmark) (* -2.5 th))));DATUM
			 (setq p17 (list (+ (car pmboxmark) (* 86 th))(+ (cadr pmboxmark)  0 )));DATE
			 (setq p18 (list (+ (car pmboxmark) (* 96 th))(+ (cadr pmboxmark) (* -2.5 th))))
			 
			 ;draw boxes
			 (command "rectangle" p10 p11)
			 (command "rectangle" p11 p12)
			 (command "rectangle" p12 p13)
			 (command "rectangle" p13 p14)
			 (command "rectangle" p14 p15)
			 (command "rectangle" p15 p16)
			 (command "rectangle" p16 p17)
			 (command "rectangle" p17 p18)
			 
			 ;text insertion points
			 (setq p20 (list (+ (car pmboxmark) (* 11 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p21 (list (+ (car pmboxmark) (* 28.5 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p22 (list (+ (car pmboxmark) (* 42 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p23 (list (+ (car pmboxmark) (* 52 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p24 (list (+ (car pmboxmark) (* 58 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p25 (list (+ (car pmboxmark) (* 67.5 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p26 (list (+ (car pmboxmark) (* 80 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p27 (list (+ (car pmboxmark) (* 91 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 
			 ;create text
			 (command "text" "j" "mc" p20 th "90" "MARK")
			 (command "text" "j" "mc" p21 th "90" "EAST")
			 (command "text" "j" "mc" p22 th "90" "NORTH")
			 (command "text" "j" "mc" p23 th "90" "HEIGHT")
			 (command "text" "j" "mc" p24 th "90" "ORDER")
			 (command "text" "j" "mc" p25 th "90" "DESCRIPTION")
			 (command "text" "j" "mc" p26 th "90" "DATUM")
			 (command "text" "j" "mc" p27 th "90" "DATE")
			 ;reset pm box mark point
			 (setq pmboxmark p10)
			 ));p&if no boxmark


  
  			;box corners
			(setq p10 (list (+ (car pmboxmark) 0)(+ (cadr pmboxmark) (* -2.5 th))));NUM
			 (setq p11 (list (+ (car pmboxmark) (* 22 th))(+ (cadr pmboxmark)  0 )));EAST
			 (setq p12 (list (+ (car pmboxmark) (* 35 th))(+ (cadr pmboxmark) (* -2.5 th))));NORTH
			 (setq p13 (list (+ (car pmboxmark) (* 49 th))(+ (cadr pmboxmark)  0 )));HEIGHT
			 (setq p14 (list (+ (car pmboxmark) (* 55 th))(+ (cadr pmboxmark) (* -2.5 th))));ORDER
			 (setq p15 (list (+ (car pmboxmark) (* 61 th))(+ (cadr pmboxmark)  0 )));SOURCE
			 (setq p16 (list (+ (car pmboxmark) (* 74 th))(+ (cadr pmboxmark) (* -2.5 th))));DATUM
			 (setq p17 (list (+ (car pmboxmark) (* 86 th))(+ (cadr pmboxmark)  0 )));DATE
	                 (setq p18 (list (+ (car pmboxmark) (* 96 th))(+ (cadr pmboxmark) (* -2.5 th))))
			 
			 ;draw boxes
			 (command "rectangle" p10 p11)
			 (command "rectangle" p11 p12)
			 (command "rectangle" p12 p13)
			 (command "rectangle" p13 p14)
			 (command "rectangle" p14 p15)
			 (command "rectangle" p15 p16)
			 (command "rectangle" p16 p17)
			 (command "rectangle" p17 p18)
			 
			 ;text insertion points
			 (setq p20 (list (+ (car pmboxmark) (* 11 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p21 (list (+ (car pmboxmark) (* 28.5 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p22 (list (+ (car pmboxmark) (* 42 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p23 (list (+ (car pmboxmark) (* 52 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p24 (list (+ (car pmboxmark) (* 58 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p25 (list (+ (car pmboxmark) (* 67.5 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p26 (list (+ (car pmboxmark) (* 80 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p27 (list (+ (car pmboxmark) (* 91 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 
			 ;create text
			 (command "text" "j" "mc" p20 th "90" pmnum)
			 (command "text" "j" "mc" p21 th "90" (rtos (atof pmeast) 2 3))
			 (command "text" "j" "mc" p22 th "90" (rtos (atof pmnorth) 2 3))
			 ;(command "text" "j" "mc" p23 th "90" class)
			 (command "text" "j" "mc" p24 th "90" pmorder)
			 (command "text" "j" "mc" p25 th "90" pmtech)
			 (command "text" "j" "mc" p26 th "90" pmdatum)
			 (command "text" "j" "mc" p27 th "90" pmdate)
			 ;reset pm box mark point
			 (setq pmboxmark p10)


  
  ;ADD MONUMENT
(IF (/= (SUBSTR PMNUM 1 3) "PCM")
  (PROGN
   (SETVAR "CLAYER"  "Monument" )
  (if (= rmtype "Occupation")(setvar "clayer" "Occupations"))
  
  (COMMAND "POINT" pmpos)

  ;check for no values and replace with "none"
  (if (= rmcondition "")(setq rmcondition "none"))
  (if (/= rmtype "")(setq ormtype (strcat "type=\"" rmtype "\""))(setq ormtype ""))
  (if (/= rmcondition "none")(setq ormcondition (strcat " condition=\"" rmcondition "\""))(setq ormcondition ""))
   

  (if (and (= rmcomment "") (= rmornot "RM")) (setq rmornot " RM"));readd space when there is no comment
  
    (SETQ PTINFO (STRCAT  ormtype " state=\""  rmstate "\"" ormcondition " />" ));Note comment for desc in xml added to distance entry seperated by a space
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 PTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

))



   (setvar "clayer" prevlayer)

  
  )
  


;------------------------------------------------------------------Create Datum Point----------------------------


(defun C:XDP (/)
   (setq prevlayer (getvar "CLAYER"))

  (setq dppos (getpoint "\nSelect Datum Point position:"))
  (setq ab (getstring "\nLetter assginment (eg A,B):"))
  
  (setq ab (strcase ab))
  
  

		 
 

		(SETVAR "CLAYER"  "Datum Points" )
		 
     (COMMAND "POINT" dppos)


  
  
		 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 ab)))))
  (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
(SETQ TEXTPOS (LIST (- (CAR dpPOS) TH) (+ (CADR dpPOS) (* 0.5 TH))))
(setq height (caddr dppos))
  
		 (SETVAR "CLAYER"  "Drafting AFR" )
  
		 

  	 (SETVAR "CLAYER"  "Drafting AFR" )
      (if (/= height 0)
  		 (progn;stratum datum point
			 (COMMAND "TEXT" "J" "BR"  TEXTPOS (* TH 1.4) "90"  ab )
			 (COMMAND "TEXT" "J" "BL"  dppos (* TH 1) "45" (rtos  height 2 3))
			 );P
	       (COMMAND "TEXT" "J" "BR"  TEXTPOS (* TH 2) "90"  (STRCAT "'" ab "'") );normal datum point
		       );IF SPCPOS2 NIL
  
  (setvar "clayer" prevlayer)
  
  )


;------------------------------------------------------------Create Occupation Offset---------------------------------------------

(defun C:XOC (/)
    (setq prevlayer (getvar "CLAYER"))

  ;(setq occobj (car (entsel "\nSelect Object to Offset:" )))
  (setq occpnt (getpoint "\nSelect Point to offset:" ))
  (setq bdyline (car (entsel "\nSelect boundary line:" )))
  (setq comment (getstring t "\nOccupation comment:" ))
  ;(SETQ layer (CDR(ASSOC 8 (ENTGET occobj))))
(if (/= comment "")(setq comment (strcat " " comment)))
  
  
(if ( = (CDR(ASSOC 0 (ENTGET bdyline))) "LINE")(PROGN
  (SETQ P1 (CDR(ASSOC 10 (ENTGET bdyline))))
  (SETQ P2 (CDR(ASSOC 11 (ENTGET bdyline))))
  (SETQ P1 (LIST (CAR P1)(CADR P1)))
  (SETQ OCCPNT (TRANS OCCPNT 1 0))
 ;check line one
;check offset to line
  (SETQ ANG (ANGLE P1 P2))
  (SETQ CANG (+ ANG (/ PI 2)))
  (SETQ P4 (POLAR occpnt CANG 50))
  (SETQ P6 (POLAR occpnt (+ CANG PI) 50))
   
   (SETQ P5 (INTERS P1 P2 P6 P4 nil))					
 
      (SETQ OFF (DISTANCE occpnt P5))
      
  (setq mp (list (/ (+ (car occpnt)(car p5)) 2)(/ (+ (cadr occpnt)(cadr p5)) 2)))

  
  (setq ang (angle (trans occpnt 0 1) (trans  p5 0 1)))
  (if (and (> ang  (* 0.5 pi))(< ang (* 1.5 pi)))(setq ang (- ang pi)))
  (if (< ang 0)(setq ang (+ ang (* 2 pi))))

  ))

    
(if ( = (CDR(ASSOC 0 (ENTGET bdyline))) "ARC")(PROGN
  (SETQ CP (CDR(ASSOC 10 (ENTGET bdyline))))
  (SETQ RADIUS (CDR(ASSOC 40 (ENTGET bdyline))))
  (SETQ CP (LIST (CAR CP) (CADR CP)))
  
  (SETQ ANG (ANGLE CP OCCPNT))

  (SETQ P5 (POLAR CP ANG RADIUS))
  

  (SETQ OFF (DISTANCE occpnt P5))
  
      
   

  (setq mp (list (/ (+ (car occpnt)(car p5)) 2)(/ (+ (cadr occpnt)(cadr p5)) 2)))
  (setq ang (angle (trans occpnt 0 1) (trans  p5 0 1)))
  (if (and (> ang  (* 0.5 pi))(< ang (* 1.5 pi)))(setq ang (- ang pi)))
  (if (< ang 0)(setq ang (+ ang (* 2 pi))))

  ))

  (IF (= QROUND "YES")
    (progn
   
(SETQ LLEN OFF)
      (IF (< LLEN DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> LLEN DISTMAX1)(< LLEN DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> LLEN DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ LLEN DROUND)))
    (SETQ LFP (- (/ LLEN DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ LLEN (* LIP DROUND))

    (SETQ OFF  LLEN)
    
    ))

  

      (setvar "clayer" "Occupations")

  (setq occpnt (trans occpnt 0 1))
  (setq p5 (trans p5 0 1))

  
      ;line based kerb occ
      
	(command "line" occpnt p5 "")
	(SETQ BDINFO (STRCAT "desc=\"(" (rtos off 2 3) COMMENT")\">"))
 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

  (if (> off (* th 5))(setq tpos mp
			    just "BC"))

					  (if (and (< off (* th 7))(>= (angle occpnt p5) (* 0.5 pi))(<= (angle occpnt p5)(* 1.5 pi)))(setq tpos p5
																	 just "BR"))
					  (if (and (< off (* th 7))(or(<= (angle occpnt p5) (* 0.5 pi))(>= (angle occpnt p5)(* 1.5 pi))))(setq tpos p5
																	 just "BL"))
	(setvar "clayer" "Drafting AFR")
	(COMMAND "TEXT" "J" JUST TPOS TH (ANGTOS ANG 1 4) (strcat "(" (rtos off 2 3)  (strcase comment) ")"))
	
	
  
;wall based monument offset
     ; (if (or (= layer "Occupation Walls")(= layer "Occupation Fences")(= layer "Occupation Buildings"))(progn
					;(command "point" occpnt)
					;(setq overclear (getstring "\nOver or Clear [C]:"))
					;(if (or (= overclear "o")(= overclear "O"))(setq overclear "Over"))
					;(if (or (= overclear "")(= overclear "c")(= overclear "C"))(setq overclear "Clear"))
  	;(SETQ BDINFO (STRCAT "type=\"Occupation\" state=\"Existing\" desc=\"" (rtos off 2 3) " " overclear "\" />"))
 ;(SETQ SENT (ENTLAST))
 ; (SETQ SENTLIST (ENTGET SENT))
  ;(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   ;(setq NEWSENTLIST (APPEND SENTLIST XDATA))
  ;(ENTMOD NEWSENTLIST)
					;(if (> off (* th 5))(setq tpos mp
					;			  just "MC"))

					 ; (if (and (< off (* th 7))(>= (angle occpnt p5) (* 0.5 pi))(<= (angle occpnt p5)(* 1.5 pi)))(setq tpos p5
				;													 just "MR"))
				;	  (if (and (< off (* th 7))(or(<= (angle occpnt p5) (* 0.5 pi))(>= (angle occpnt p5)(* 1.5 pi))))(setq tpos p5
				;													 just "ML"))

				;	  (setvar "clayer" "Drafting")
	;(COMMAND "TEXT" "J" just tpos TH (ANGTOS ANG 1 4) (strcat "(" (rtos off 2 3) " " (substr overclear 1 2) ")"))
	;				  ));p&if wall or fence
				

     
(setvar "clayer" prevlayer)

  
  )




;-------------------------------------------------------------------DRAW CHAINAGE-------------------------------------
(defun C:XTC (/)

  (setq entss (ssadd))
  (setq chainlist "00")
  (setq prevlayer (getvar "CLAYER"))

  ;GET 1ST LINE INFO
    (graphscr)
    
    (setq p1 (getpoint "\nEnter start coords: "))
    (setq bearing (getstring "\nBearing(DD.MMSS): "))
  (setq obearing bearing)
   (setq dist (getstring T (strcat "\nDistance[Meters/Feet/DecimalFeet/Links]" units ":")))

  (if (or (= dist "f") (= dist "F") (= dist "Feet") (= dist "feet") (= dist "FEET"))
    (progn
      (setq dist (getstring T "\nDistance(Feet FF.II.n/d ): "))
      (setq units "F")
      )
    )
  
   (if (or (= dist "d")(= dist "df")(= dist "DF") (= dist "D") (= dist "DecimalFeet") (= dist "decimalfeet") (= DIST "DECIMALFEET"))
    (progn
      (setq dist (getstring T "\nDistance(Decimal Feet): "))
      (setq units "DF")
      )
    )

  (if (or (= dist "l") (= dist "L") (= dist "Links") (= dist "LINKS") (= DIST "links"))
    (progn
      (setq dist (getstring T "\nDistance(Links): "))
      (setq units "L")
      )
    )

    (if (or (= dist "m") (= dist "M") (= dist "Meters") (= dist "meters") (= DIST "METERS"))
    (progn
      (setq dist (getstring T "\nDistance(Meters): "))
      (setq units "M")
      )
    )

  
  (setq prevdist dist)
 
 ;APPLY ALL CORRECTIONS AND EXTRACT INFORMATION FROM USER INPUT

  ;reverse
  (if (or (= (substr bearing 1 1 ) "r") (= (substr bearing 1 1 ) "R" ))
    (progn
      (setq bearing (substr bearing 2 50))
      (setq rswitch "T")
      )
     (setq rswitch "F")
    )

;look for cardinals
  (setq card1 ""
	card2 "")
  (IF (OR (= (substr bearing 1 1 ) "n") (= (substr bearing 1 1 ) "N" )
	  (= (substr bearing 1 1 ) "s" )(= (substr bearing 1 1 ) "S" )
	  (= (substr bearing 1 1 ) "e" )(= (substr bearing 1 1 ) "E" )
	  (= (substr bearing 1 1 ) "w" )(= (substr bearing 1 1 ) "W" ))
(progn
    (setq card1 (substr bearing 1 1))
    (setq card2 (substr bearing (strlen bearing) 1))
    (setq bearing (substr bearing 2 (- (strlen bearing )2)))
  )
    )
    
  
(if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
  (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins  (strcat (substr bearing (+ dotpt1 2) 2) (chr 39)))
  (setq sec  (substr bearing (+ dotpt1 4) 10))

  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))

  
  (if (or
	(= (strlen sec) 2)
	(= (strlen mins) 2)
	(> (atof mins) 60)
	(> (atof sec) 60)
	(> (atof deg) 360)
	)
    (alert (strcat "That bearing looks a little funky - " bearing)))
  
  
  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

 ;correct cardinals

    (IF  (and (or (= card1 "n")(= card1 "N")(= card1 "s")(= card1 "S"))(and (/= card2 "w")(/= card2 "W")(/= card2 "e")(/= card2 "E")))
	  (alert (strcat "Cardinal missing E or W"))
    )
  
  (if (and (or (= card1 "n")(= card1 "N"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (- 360 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );NW

		   

		   (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (- 270 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );WS

		   
		   (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (- 90 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );EN

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "e")(= card2 "E")))
    (progn
      (setq deg (rtos (- 180 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );SE

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (+ 180 (atof deg)) 2 0))
      
      )
    );SW

		    (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (+ 270 (atof deg)) 2 0))
      
      )
    );WN
		    (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (+ 90 (atof deg)) 2 0))
      
      )
    );ES

  		   (if (and (= mins "")(= sec ""))(setq decimal "")(setq decimal "."))
(setq lbearing (strcat deg decimal (substr mins 1 2) (substr sec 1 2)))
   (if (= rswitch "T")(setq lbearing (strcat "R" lbearing)))
  (IF ( = rswitch "T")(setq obearing (substr lbearing 2 200))(setq obearing lbearing))



  ;look for line comment
  (setq spcpos1 (vl-string-position 32 dist 0))
	(if (= spcpos1 nil)(setq comment "")(progn
					      (setq comment (substr dist ( + spcpos1 2) 50))
					      (setq dist (substr dist 1  spcpos1 ))
					      )
	  )


  
    (if (= units "F")
      (progn
	 (setq dotpos1 (vl-string-position 46 dist 0)) 
		    
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 dist (+ dotpos1 1))))
	(setq /pos1 (vl-string-position 47 dist 0))
	(if (/= /pos1 nil)
	  (progn
	    (setq den (substr dist ( + /pos1 2) 50))
	    (setq num (substr dist ( + dotpos2 2) (- (- /pos1 dotpos2) 1)))
	    (setq idist (/ (atof num) (atof den)))
	    (setq inches (substr dist (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq idist (+ idist (atof inches)))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (/= dotpos1 nil) (= /pos1 nil))
	  (progn
	    (setq inches (substr dist ( + dotpos1 2) 50))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist  (atof inches))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (= dotpos1 nil) (= /pos1 nil) (= dotpos2 nil))
	  (progn
	   
	    (setq feet (substr dist 1  50))
	    (setq idist (* (atof feet) 12))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
      )
    )
  (if (= units "L")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.201168)))
      )
    )
  (if (= units "DF")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.3048)))
      )
    )
	      
	      

    ;DRAW LINE 1
     
  
  (setq dist (rtos (atof dist)2 3));remove trailing zeros

  (setq chainlist  (strcat chainlist ","  dist))
  
  (if (= dist "0") (progn
		     (princ "\nDistance of 0 entered")
		     (exit))
    )
  (setq ldist dist)
  (SETQ lastdist dist)
  ;(if (= rswitch "T")(progn
;		       (setq deg (+ (atof deg) 180))
;		       (if (> deg 360)(setq deg (- deg 360)))
;		       (setq deg (rtos deg 2 0))
;		       ))
  (setq bearing (strcat  deg "d" mins sec))

  ;(setvar "clayer" "Occupations")
  (setq linetext (strcat "@" dist "<" bearing))
    (command "line" p1 linetext "")
    
  (if (/= comment "")(setq ocomment (strcat "><FieldNote>\"" comment "\"</FieldNote></ReducedObservation>"))(setq ocomment "/>"))
(SETQ BDINFO (STRCAT "azimuth=\"" obearing "\" horizDistance=\"" ldist "\"" ocomment))
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

 

 (SETQ RMB (ENTLAST))
  (SSADD RMB ENTSS)

  
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (setq sp (CDR(ASSOC 10 sentlist)))
(setq p1 (CDR(ASSOC 11 sentlist)))

   (if (= rswitch "T")
  (progn
    (setq p1 (CDR(ASSOC 10 sentlist)))
    (setq p2 (CDR(ASSOC 11 sentlist)))
    (command "move" sent "" p2 p1)
    ;get last line end point
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 10 sentlist)))
    (setq sp (CDR(ASSOC 11 sentlist)))
    ))
  
    (SETQ CP1 (TRANS  sp 0 1))
    (SETQ CP2 (TRANS  p1 0 1))
    (SETQ ANG (- (ANGLE CP1 CP2) (* 0.5 pi)))
    (SETQ DANG (* ANG (/ 180 PI)))

    (IF (AND (> DANG 90) (<= DANG 270)) (PROGN
					 (SETQ CP1 (TRANS P1 0 1))
					 (SETQ CP2 (TRANS SP 0 1))
					 (SETQ ANG (- (ANGLE CP1 CP2) (* 0.5 PI)))
                                         (SETQ DANG (* ANG (/ 180 PI)))
					 )
      )

   
   

  (if (/= comment "")(setq comment (strcat " "comment)))

  
   (SETVAR "CLAYER"  "Drafting AFR" )
  (setq 00pos (polar sp (- (angle cp1 cp2) (* 0.5 pi)) (* 0.7 TH)))
    (command "insert" "VCH0" "_S" TH 00pos (angtos (ANGLE (trans sp 0 1)(trans p1 0 1)) 1 4))
    (COMMAND "TEXT" "J" "ML" P1 TH (ANGTOS ANG 1 4) (strcat dist (strcase comment)))
;label ye old dist
  (if (or (= units "F")(= units "L")(= units "DF"))
    (progn
      (setq textstyle (getvar "textstyle"))
(SETQ textfont (ENTGET (tblobjname "style" textstyle)))
(setq theElist (subst (cons 50 (* 20 (/ PI 180)))(assoc 50 theElist) textfont));make ye old distances slanty
(entmod theElist)
  (COMMAND "TEXT" "J" "ML" (POLAR P1 (- ANG (* pi 0.5)) (* 1.2 TH)) TH (ANGTOS ANG 1 4) (strcat prevdist))
  (setq theElist (subst (cons 50 0)(assoc 50 theElist) textfont));set slanty back to straight
(entmod theElist)
      ))
    
  
  

  
(setvar "clayer" prevlayer)

  ;GET 2ND+ LINE INFO
  (while (> 1 0) (progn
		  

        (setq dist (getstring T (strcat "\nChainage" units ":")))

		   
		   (setq prevdist dist)


 ;APPLY ALL CORRECTIONS AND EXTRACT INFORMATION FROM USER INPUT


	

  ;look for line comment
  (setq spcpos1 (vl-string-position 32 dist 0))
	(if (= spcpos1 nil)(setq comment "")(progn
					      (setq comment (substr dist ( + spcpos1 2) 50))
					      (setq dist (substr dist 1  spcpos1 ))
					      )
	  )


  
    (if (= units "F")
      (progn
	 (setq dotpos1 (vl-string-position 46 dist 0)) 
		    
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 dist (+ dotpos1 1))))
	(setq /pos1 (vl-string-position 47 dist 0))
	(if (/= /pos1 nil)
	  (progn
	    (setq den (substr dist ( + /pos1 2) 50))
	    (setq num (substr dist ( + dotpos2 2) (- (- /pos1 dotpos2) 1)))
	    (setq idist (/ (atof num) (atof den)))
	    (setq inches (substr dist (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq idist (+ idist (atof inches)))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (/= dotpos1 nil) (= /pos1 nil))
	  (progn
	    (setq inches (substr dist ( + dotpos1 2) 50))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist  (atof inches))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (= dotpos1 nil) (= /pos1 nil) (= dotpos2 nil))
	  (progn
	   
	    (setq feet (substr dist 1  50))
	    (setq idist (* (atof feet) 12))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
      )
    )
  (if (= units "L")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.201168)))
      )
    )
	(if (= units "DF")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.3048)))
      )
    )
	      
	      


	
		;DRAW LINE 2+

		    (setq dist (rtos (atof dist)2 3));remove trailing zeros

	
		   (if (or (= dist "")(= dist "0")) (progn ;if no dist run exiter, join polylines, label bearing
				(command "pedit" "m" entss "" "y" "j" "" "")

			
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 chainlist)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
(SETQ NEWSENTLIST (subst (cons 8 "Occupations")(assoc 8 NEWSENTLIST) NEWSENTLIST ))
  (ENTMOD NEWSENTLIST)

				(if (= rswitch "T") (command "reverse" sent ""))
				
				
				
 (SETQ CP1 (TRANS  sp 0 1))
    (SETQ CP2 (TRANS  p1 0 1))
    (SETQ ANG (ANGLE CP1 CP2))
    (SETQ DANG (* ANG (/ 180 PI)))

    (IF (AND (> DANG 90) (<= DANG 270)) (PROGN
					 (SETQ CP1 (TRANS p1 0 1))
					 (SETQ CP2 (TRANS sp 0 1))
					 (SETQ ANG (ANGLE CP1 CP2))
                                         (SETQ DANG (* ANG (/ 180 PI)))
					 )
      )
 (SETQ MPE (/ (+ (CAR CP1 ) (CAR CP2)) 2))
    (SETQ MPN (/ (+ (CADR CP1 ) (CADR CP2)) 2))
    (SETQ MP (LIST MPE MPN))
    (SETQ BPOS (POLAR MP (+ ANG (* 0.5 PI)) TH))
    (SETQ DPOS (POLAR MP (- ANG (* 0.5 PI)) TH))
    (SETQ BTS (vl-string-subst  "°" "d" bearing))

  (if (/= comment "")(setq comment (strcat " "comment)))

  (setq prevlayer (getvar "CLAYER"))
   (SETVAR "CLAYER"  "Drafting AFR" )
    (COMMAND "TEXT" "J" "MC" BPOS TH (ANGTOS ANG 1 4) BTS)
				

				(setvar "clayer" prevlayer)		      
		     
		     (exit))
    );if no dist
	
  (setq ldist dist)
	(setq drawdist (rtos (- (atof dist) (atof lastdist))))
	   (setq lastdist drawdist)
    

  (setq linetext (strcat "@" drawdist "<" bearing))
	
;(SETVAR "CLAYER"  "Occupations" )
  (command "line" p1 linetext "")
 (if (/= comment "")(setq ocomment (strcat "><FieldNote>\"" comment "\"</FieldNote></ReducedObservation>"))(setq ocomment "/>"))
(SETQ BDINFO (STRCAT "azimuth=\"" obearing "\" horizDistance=\"" drawdist "\""  ocomment))
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)


	
	
	(setq lastdist dist)
	(setq chainlist  (strcat chainlist ","  drawdist))

	(SETQ RMB (ENTLAST))
  (SSADD RMB ENTSS)

;get last line end point
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 11 sentlist)))

		(if (= rswitch "T")
  (progn
    (setq p1 (CDR(ASSOC 10 sentlist)))
    (setq p2 (CDR(ASSOC 11 sentlist)))
    (command "move" sent "" p2 p1)
    ;get last line end point
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(setq p1 (CDR(ASSOC 10 sentlist)))    
    ))
    
         

  (if (/= comment "")(setq comment (strcat " "comment)))

  
   (SETVAR "CLAYER"  "Drafting AFR" )
   
    (COMMAND "TEXT" "J" "ML" P1 TH (ANGTOS ANG 1 4) (strcat dist (strcase comment)))

	;label ye old dist
	 (if (or (= units "F")(= units "L")(= units "DF"))
    (progn
      (setq textstyle (getvar "textstyle"))
(SETQ textfont (ENTGET (tblobjname "style" textstyle)))
(setq theElist (subst (cons 50 (* 20 (/ PI 180)))(assoc 50 theElist) textfont));make ye old distances slanty
(entmod theElist)
  (COMMAND "TEXT" "J" "ML" (POLAR P1 (- ANG (* pi 0.5)) (* 1.2 TH)) TH (ANGTOS ANG 1 4) (strcat prevdist))
  (setq theElist (subst (cons 50 0)(assoc 50 theElist) textfont));set slanty back to straight
(entmod theElist)
      ))
	   
(setvar "clayer" prevlayer)
		   
  
);P
    );WHILE 1>0
  );DEFUN




;-----------------------------------------------------------------------------Assign Polyline to CHAINAGE----------------------------------------------------
(defun C:XAC (/)

  (setq chainlist "00")
  (setq prevlayer (getvar "CLAYER"))

 (SETQ SENT (CAR (ENTSEL "\nSelect Polyline:")))
  
  (SETQ SENTLIST (ENTGET SENT))

  (SETQ PTLIST (LIST))
				
;CREATE LIST OF POLYLINE POINTS
    (SETQ PTLIST (LIST))
	    (foreach a SENTLIST
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	     
	    )				;FOREACH 			)



   (setq sp (nth 0 ptlist))
(setq p1 (nth 1 ptlist))
(setq LLEN (distance sp p1))

;ROUND DISTANCES

    (IF (< LLEN DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> LLEN DISTMAX1)(< LLEN DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> LLEN DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ LLEN DROUND)))
    (SETQ LFP (- (/ LLEN DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ LLEN (* LIP DROUND))

    (SETQ LDIST (RTOS LLEN 2 3))
    
  

(setq chaindist LLEN)
(setq chainlist  (strcat chainlist ","  LDIST))
  
    (SETQ CP1 (TRANS  sp 0 1))
    (SETQ CP2 (TRANS  p1 0 1))
    (SETQ ANG (- (ANGLE CP1 CP2) (* 0.5 pi)))
    (SETQ DANG (* ANG (/ 180 PI)))

    (IF (AND (> DANG 90) (<= DANG 270)) (PROGN
					 (SETQ CP1 (TRANS P1 0 1))
					 (SETQ CP2 (TRANS SP 0 1))
					 (SETQ ANG (- (ANGLE CP1 CP2) (* 0.5 PI)))
                                         (SETQ DANG (* ANG (/ 180 PI)))
					 )
      )

   
  
   (SETVAR "CLAYER"  "Drafting AFR" )
  (setq 00pos (polar sp (- (angle cp1 cp2) (* 0.5 pi)) (* 0.7 TH)))
    (command "insert" "VCH0" "_S" TH 00pos (angtos (ANGLE (trans sp 0 1)(trans p1 0 1)) 1 4))
    (COMMAND "TEXT" "J" "ML" P1 TH (ANGTOS ANG 1 4)  ldist )

  (SETQ COUNT 1)
  (REPEAT (- (LENGTH PTLIST) 2)
    (setq p1 (nth count ptlist))
    (setq p2 (nth (+ count 1) ptlist))
    (setq dist (distance p1 p2))
    (setq chaindist (+ chaindist dist))
    (SETQ LLEN CHAINDIST)
	  
;ROUND DISTANCES

    (IF (< LLEN DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> LLEN DISTMAX1)(< LLEN DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> LLEN DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ LLEN DROUND)))
    (SETQ LFP (- (/ LLEN DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ LLEN (* LIP DROUND))

    (SETQ LDIST (RTOS LLEN 2 3))
    
    ;(IF (< LLEN 1) (SETQ DTS (STRCAT "0" DTS)))
    
    (COMMAND "TEXT" "J" "ML" P2 TH (ANGTOS ANG 1 4) ldist )
    (SETQ COUNT (+ COUNT 1))

    (setq chainlist  (strcat chainlist ","  (rtos DIST 2 3)))
    
    );R




   (SETQ ANG (ANGLE  SP  P2 ))

  (SETQ BEARING (ANGTOS ANG 1 4));REQUIRED FOR ELSE ROUND
  (setq bearing (vl-string-subst "d" (chr 176) bearing));added for BricsCAD changes degrees to "d"
    

  (IF (= QROUND "YES")(PROGN
  
 

    ;ASSIGN ROUNDING FOR ANGLES BASED ON DISTANCE
    (IF (< CHAINDIST MAXLEN1) (SETQ ROUND BRND1))
    (IF (AND (> CHAINDIST MAXLEN1)(< CHAINDIST MAXLEN2)) (SETQ ROUND BRND2))
    (IF (> CHAINDIST MAXLEN2)(SETQ ROUND BRND3))			
			
    ;(IF (> LLEN 100) (SETQ ROUND 1))

   
    ;GET ANGLE DELIMIETERS
    (SETQ SANG (ANGTOS ANG 1 4))
    (setq sang (vl-string-subst "d" (chr 176) sang));added for BricsCAD changes degrees to "d"
    (setq CHRDPOS (vl-string-position 100 SANG 0))
    (setq MINPOS (vl-string-position 39 SANG 0))
    (setq SECPOS (vl-string-position 34 SANG 0))

    ;PARSE ANGLE
    (setq DEG  (atof (substr SANG 1  CHRDPOS )))
    (setq MINS  (atof (substr SANG (+ CHRDPOS 2)  (-(- MINPOS CHRDPOS)1))))
    (setq SEC  (atof (substr SANG (+ MINPOS 2)  (-(- SECPOS MINPOS )1))))

   
;ROUND ANGLE, NOTE SECONDS REMOVED
    (IF (and (= ROUND 60)(< SEC 30)) (SETQ SEC 0))
    (IF (and (= ROUND 60)(>= SEC 30)) (SETQ SEC 0
					    MINS (+ MINS 1)))					  
    (IF (/= ROUND 60) (PROGN
			(SETQ SIP (FIX (/ SEC ROUND)))
			(SETQ SFP (- (/  SEC ROUND) SIP))
			(IF (>= SFP 0.5) (SETQ SIP (+ SIP 1)))
			(SETQ SEC (* SIP ROUND))
			)
      )

    
		 
    
;STRING ANGLES
    (SETQ DEG (RTOS DEG 2 0))
    (SETQ MINS (RTOS MINS 2 0))
    (SETQ SEC (RTOS SEC 2 0))
    
;INCREMENT IF SECONDS ROUNDED TO 60
    (IF (= SEC  "60")
      (PROGN
	(SETQ SEC "00")
	(SETQ MINS (RTOS (+ (ATOF MINS ) 1) 2 0))
	)
      )
;INCREMENT IF MINUTES ROUNDED TO 60
    (IF (= MINS "60")
      (PROGN
	(SETQ MINS "00")
	(SETQ DEG (RTOS (+ (ATOF DEG ) 1) 2 0))
	)
      )
;FIX IF INCREMENTING PUSHES DEG PAST 360    
    (IF (= DEG "360")(SETQ DEG "0"))
;ADD ZEROS TO SINGLE NUMBERS	
 (IF (= (STRLEN MINS) 1)(SETQ MINS (STRCAT "0" MINS)))
  (IF (= (STRLEN SEC) 1)(SETQ SEC (STRCAT "0" SEC)))

;TRUNCATE BEARINGS IF 00'S
  (IF (AND (= MINS "00") (= SEC "00")) (SETQ MINSS ""
					     MINS ""
					     SECS ""
					     SEC "")
        (SETQ MINSS(STRCAT MINS "'")
	  SECS (STRCAT SEC "\""))
	  )
    (IF (= SEC "00")(SETQ SECS ""
			    SEC ""))

			

    ;CONCATENATE BEARING
    (SETQ BEARING (STRCAT DEG "d" MINSS SECS ))

			(IF (or (/= sec "")(/= MINS ""))(SETQ DEG (STRCAT DEG ".")))

  (SETQ LBEARING (STRCAT DEG MINS SEC))

			
    

			);P&IF


(PROGN;ELSE

  

  (SETQ DPOS (vl-string-position 100 BEARING 0))
  (setq Wpos (vl-string-position 39 BEARING 0))
  (setq WWpos (vl-string-position 34 BEARING 0))

    (setq DEG (substr BEARING 1 Dpos))
      (setq MINS (substr BEARING (+ Dpos 2) (- (- WPOS DPOS) 1)))
      (setq SEC (substr BEARING (+ Wpos 2) (- (- WWpos Wpos) 1)))

  (IF (= (STRLEN MINS) 1)(SETQ MINS (STRCAT "0" MINS)))
  (IF (= (STRLEN SEC) 1)(SETQ SEC (STRCAT "0" SEC)))
  
  (IF (AND (= MINS "00") (= SEC "00")) (SETQ MINSS ""
					     SECS ""
					     MINS ""
					     SEC "")
    (SETQ MINSS(STRCAT MINS "'")
	  SECS (STRCAT SEC "\""))
	  	  )
  (IF (= SECS "00\"")(SETQ SECS ""
			   SEC ""))
  
  (SETQ BEARING (STRCAT DEG "d" MINSS SECS ))

  	(IF (or (/= sec "")(/= MINS ""))(SETQ DEG (STRCAT DEG ".")))

  
  ));PELSE&IF


  
    				
 (SETQ CP1 (TRANS  sp 0 1))
    (SETQ CP2 (TRANS  p2 0 1))
    (SETQ ANG (ANGLE CP1 CP2))
    (SETQ DANG (* ANG (/ 180 PI)))

    (IF (AND (> DANG 90) (<= DANG 270)) (PROGN
					 (SETQ CP1 (TRANS p2 0 1))
					 (SETQ CP2 (TRANS sp 0 1))
					 (SETQ ANG (ANGLE CP1 CP2))
                                         (SETQ DANG (* ANG (/ 180 PI)))
					 )
      )
 (SETQ MPE (/ (+ (CAR CP1 ) (CAR CP2)) 2))
    (SETQ MPN (/ (+ (CADR CP1 ) (CADR CP2)) 2))
    (SETQ MP (LIST MPE MPN))
    (SETQ BPOS (POLAR MP (+ ANG (* 0.5 PI)) TH))
    (SETQ DPOS (POLAR MP (- ANG (* 0.5 PI)) TH))
    (SETQ BTS (vl-string-subst  "°" "d" bearing))

 
  (setq prevlayer (getvar "CLAYER"))
   (SETVAR "CLAYER"  "Drafting AFR" )
    (COMMAND "TEXT" "J" "MC" BPOS TH (ANGTOS ANG 1 4) BTS)


  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 chainlist)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
  
  (setvar "clayer" prevlayer)

  )



;-----------------------------------------------------------------------------CREATE CHAINAGE----------------------------------------------------
(defun C:XCC (/)

  (setq chainlist "00")
  (setq prevlayer (getvar "CLAYER"))

 (SETQ SENT (CAR (ENTSEL "\nSelect Occupation Polyline:")))
  (setq bearing (getstring "\nBearing of line(or enter to select line):"))
  (if (= bearing "")(progn
		      (SETQ BENT (entget (CAR (ENTSEL "\nSelect Line:"))))
		      (setq p1 (cdr (assoc 10 bent)))
		      (setq p2 (cdr (assoc 11 bent)))
		       (SETQ ANG (ANGLE  P1  P2 ))

  (SETQ BEARING (ANGTOS ANG 1 4));REQUIRED FOR ELSE ROUND
  (setq bearing (vl-string-subst "d" (chr 176) bearing));added for BricsCAD changes degrees to "d"

		      ;adding bearing calculator
		      )

		      (progn

     ;reverse
  (if (or (= (substr bearing 1 1 ) "r") (= (substr bearing 1 1 ) "R" ))
    (progn
      (setq bearing (substr bearing 2 50))
      (setq rswitch "T")
      )
     (setq rswitch "F")
    )

;look for cardinals
  (setq card1 ""
	card2 "")
  (IF (OR (= (substr bearing 1 1 ) "n") (= (substr bearing 1 1 ) "N" )
	  (= (substr bearing 1 1 ) "s" )(= (substr bearing 1 1 ) "S" )
	  (= (substr bearing 1 1 ) "e" )(= (substr bearing 1 1 ) "E" )
	  (= (substr bearing 1 1 ) "w" )(= (substr bearing 1 1 ) "W" ))
(progn
    (setq card1 (substr bearing 1 1))
    (setq card2 (substr bearing (strlen bearing) 1))
    (setq bearing (substr bearing 2 (- (strlen bearing )2)))
  )
    )
    
  
(if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
  (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins  (strcat (substr bearing (+ dotpt1 2) 2) (chr 39)))
  (setq sec  (substr bearing (+ dotpt1 4) 10))

  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))

  
  (if (or
	(= (strlen sec) 2)
	(= (strlen mins) 2)
	(> (atof mins) 60)
	(> (atof sec) 60)
	(> (atof deg) 360)
	)
    (alert (strcat "That bearing looks a little funky - " bearing)))
  
  
  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

 ;correct cardinals

    (IF  (and (or (= card1 "n")(= card1 "N")(= card1 "s")(= card1 "S"))(and (/= card2 "w")(/= card2 "W")(/= card2 "e")(/= card2 "E")))
	  (alert (strcat "Cardinal missing E or W"))
    )
  
  (if (and (or (= card1 "n")(= card1 "N"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (- 360 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );NW

		   

		   (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (- 270 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );WS

		   
		   (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (- 90 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );EN

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "e")(= card2 "E")))
    (progn
      (setq deg (rtos (- 180 (atof deg)) 2 0))
      (if (< 0 (atof mins))(progn
			     (setq mins (strcat (rtos (- 60 (atof mins)) 2 0) (chr 39)))
			     (setq deg (rtos (- (atof deg) 1) 2 0))
			     (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
				)
	     )
	(if (< 0 (atof sec))
	       (progn
		 (setq sec (strcat (rtos (- 60 (atof sec)) 2 0) (chr 34)))
		 (if (= 0 (atof mins)) (setq deg (rtos (- (atof deg) 1) 2 0)))
		 (setq mins (strcat (rtos (- (atof mins) 1) 2 0) (chr 39)))
	       
		 (if (< (atof deg) 0)(setq deg (rtos (+ (atof deg) 360) 2 0)))
		 (if (< (atof mins) 0)(setq mins (strcat (rtos (+ (atof mins) 60) 2 0)(chr 39))))
		 )
	  )
      	           
(if (or (and (< (atof mins) 10)(/= 0 (atof mins))) (= mins "0'")) (setq mins (strcat "0" mins)))
(if (and (< (atof sec) 10)(/= 0 (atof sec))) (setq sec (strcat "0" sec)))
		 
      )
    );SE

  (if (and (or (= card1 "s")(= card1 "S"))(or (= card2 "w")(= card2 "W")))
    (progn
      (setq deg (rtos (+ 180 (atof deg)) 2 0))
      
      )
    );SW

		    (if (and (or (= card1 "w")(= card1 "W"))(or (= card2 "n")(= card2 "N")))
    (progn
      (setq deg (rtos (+ 270 (atof deg)) 2 0))
      
      )
    );WN
		    (if (and (or (= card1 "e")(= card1 "E"))(or (= card2 "s")(= card2 "S")))
    (progn
      (setq deg (rtos (+ 90 (atof deg)) 2 0))
      
      )
    );ES
  
		   

  (setq lbearing (strcat deg "." (substr mins 1 2) (substr sec 1 2)))
  (if (= rswitch "T")(setq lbearing (strcat "R" lbearing)))
  


  (setq bearing (strcat  deg "d" mins sec))
    (setq linetext (strcat "@" "10" "<" bearing))
    (command "line" p1 linetext "")
  
  (SETQ BENT (entget (entlast)))
		      (setq p1 (cdr(assoc 10 bent)))
		      (setq p2 (cdr(assoc 11 bent)))
		       (SETQ ANG (ANGLE  P1  P2 ))
  (command "erase" (entlast) "")
  
);p else no bearing entered
		      );if no bearing entered

  


   (SETQ SENTLIST (ENTGET SENT))

  (SETQ PTLIST (LIST))
				
;CREATE LIST OF POLYLINE POINTS
    (SETQ PTLIST (LIST))
	    (foreach a SENTLIST
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	     
	    )				;FOREACH 			)

  


   (setq sp (nth 0 ptlist))

  (setq ptlist2 (list sp))

  ;go through list and get chainages on bearing
  (setq count 1)
  (repeat (- (length ptlist) 1)
    (setq p2 (nth count ptlist))
    (setq intpt (inters sp (polar sp ang 100) p2 (polar p2 (+ ang (* pi 0.5)) 100) nil))
    (setq ptlist2 (append ptlist2 (LIST intpt)))
    (setq count (+ count 1))
    )
	(setq ptlist ptlist2)
(setvar "clayer" "Occupations")

  (SETQ COUNT 0)
  (setq optlist2 nil)
  (REPEAT (LENGTH PTLIST2)
    (SETQ optlist2 (append optlist2 (list (trans (nth count ptlist2) 0 1))))
    (setq count (+ count 1))
    )
    
  (command "_PLINE")
     (mapcar 'command oPTLIST2)
      (command NIL)
  (setq sentlist (entget (entlast)))

  ;get first segment
   (setq sp (nth 0 ptlist))
  (setq p1 (nth 1 ptlist))
(setq LLEN (distance sp p1))
  
;ROUND DISTANCES

    (IF (< LLEN DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> LLEN DISTMAX1)(< LLEN DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> LLEN DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ LLEN DROUND)))
    (SETQ LFP (- (/ LLEN DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ LLEN (* LIP DROUND))

    (SETQ LDIST (RTOS LLEN 2 3))
    
  

(setq chaindist LLEN)
(setq chainlist  (strcat chainlist ","  LDIST))
  
    (SETQ CP1 (TRANS  sp 0 1))
    (SETQ CP2 (TRANS  p1 0 1))
    (SETQ ANG (- (ANGLE CP1 CP2) (* 0.5 pi)))
    (SETQ DANG (* ANG (/ 180 PI)))

    (IF (AND (> DANG 90) (<= DANG 270)) (PROGN
					 (SETQ CP1 (TRANS P1 0 1))
					 (SETQ CP2 (TRANS SP 0 1))
					 (SETQ ANG (- (ANGLE CP1 CP2) (* 0.5 PI)))
                                         (SETQ DANG (* ANG (/ 180 PI)))
					 )
      )

   
  
   (SETVAR "CLAYER"  "Drafting AFR" )
  (setq 00pos (polar sp (- (angle cp1 cp2) (* 0.5 pi)) (* 0.7 TH)))
    (command "insert" "VCH0" "_S" TH (trans 00pos 0 1) (angtos (ANGLE (trans sp 0 1)(trans p1 0 1)) 1 4))
    (COMMAND "TEXT" "J" "ML" (trans P1 0 1) TH (ANGTOS ANG 1 4)  ldist )

  ;continue for reamining segements
  (SETQ COUNT 1)
  (REPEAT (- (LENGTH PTLIST) 2)
    (setq p1 (nth count ptlist))
    (setq p2 (nth (+ count 1) ptlist))
    (setq dist (distance p1 p2))
    (setq chaindist (+ chaindist dist))
    (SETQ LLEN CHAINDIST)
	  
;ROUND DISTANCES

    (IF (< LLEN DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> LLEN DISTMAX1)(< LLEN DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> LLEN DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ LLEN DROUND)))
    (SETQ LFP (- (/ LLEN DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ LLEN (* LIP DROUND))

    (SETQ LDIST (RTOS LLEN 2 3))
    
    ;(IF (< LLEN 1) (SETQ DTS (STRCAT "0" DTS)))
    
    (COMMAND "TEXT" "J" "ML" (trans P2 0 1) TH (ANGTOS ANG 1 4) ldist )
    (SETQ COUNT (+ COUNT 1))

    (setq chainlist  (strcat chainlist ","  (rtos DIST 2 3)))
    
    );R




   (SETQ ANG (ANGLE  SP  P2 ))

  (SETQ BEARING (ANGTOS ANG 1 4));REQUIRED FOR ELSE ROUND
  (setq bearing (vl-string-subst "d" (chr 176) bearing));added for BricsCAD changes degrees to "d"
    

  (IF (= QROUND "YES")(PROGN
  
 

    ;ASSIGN ROUNDING FOR ANGLES BASED ON DISTANCE
    (IF (< CHAINDIST MAXLEN1) (SETQ ROUND BRND1))
    (IF (AND (> CHAINDIST MAXLEN1)(< CHAINDIST MAXLEN2)) (SETQ ROUND BRND2))
    (IF (> CHAINDIST MAXLEN2)(SETQ ROUND BRND3))			
			
    ;(IF (> LLEN 100) (SETQ ROUND 1))

   
    ;GET ANGLE DELIMIETERS
    (SETQ SANG (ANGTOS ANG 1 4))
    (setq sang (vl-string-subst "d" (chr 176) sang));added for BricsCAD changes degrees to "d"
    (setq CHRDPOS (vl-string-position 100 SANG 0))
    (setq MINPOS (vl-string-position 39 SANG 0))
    (setq SECPOS (vl-string-position 34 SANG 0))

    ;PARSE ANGLE
    (setq DEG  (atof (substr SANG 1  CHRDPOS )))
    (setq MINS  (atof (substr SANG (+ CHRDPOS 2)  (-(- MINPOS CHRDPOS)1))))
    (setq SEC  (atof (substr SANG (+ MINPOS 2)  (-(- SECPOS MINPOS )1))))

   
;ROUND ANGLE, NOTE SECONDS REMOVED
    (IF (and (= ROUND 60)(< SEC 30)) (SETQ SEC 0))
    (IF (and (= ROUND 60)(>= SEC 30)) (SETQ SEC 0
					    MINS (+ MINS 1)))					  
    (IF (/= ROUND 60) (PROGN
			(SETQ SIP (FIX (/ SEC ROUND)))
			(SETQ SFP (- (/  SEC ROUND) SIP))
			(IF (>= SFP 0.5) (SETQ SIP (+ SIP 1)))
			(SETQ SEC (* SIP ROUND))
			)
      )

    
		 
    
;STRING ANGLES
    (SETQ DEG (RTOS DEG 2 0))
    (SETQ MINS (RTOS MINS 2 0))
    (SETQ SEC (RTOS SEC 2 0))
    
;INCREMENT IF SECONDS ROUNDED TO 60
    (IF (= SEC  "60")
      (PROGN
	(SETQ SEC "00")
	(SETQ MINS (RTOS (+ (ATOF MINS ) 1) 2 0))
	)
      )
;INCREMENT IF MINUTES ROUNDED TO 60
    (IF (= MINS "60")
      (PROGN
	(SETQ MINS "00")
	(SETQ DEG (RTOS (+ (ATOF DEG ) 1) 2 0))
	)
      )
;FIX IF INCREMENTING PUSHES DEG PAST 360    
    (IF (= DEG "360")(SETQ DEG "0"))
;ADD ZEROS TO SINGLE NUMBERS	
 (IF (= (STRLEN MINS) 1)(SETQ MINS (STRCAT "0" MINS)))
  (IF (= (STRLEN SEC) 1)(SETQ SEC (STRCAT "0" SEC)))

;TRUNCATE BEARINGS IF 00'S
  (IF (AND (= MINS "00") (= SEC "00")) (SETQ MINSS ""
					     MINS ""
					     SECS ""
					     SEC "")
        (SETQ MINSS(STRCAT MINS "'")
	  SECS (STRCAT SEC "\""))
	  )
    (IF (= SEC "00")(SETQ SECS ""
			    SEC ""))

			

    ;CONCATENATE BEARING
    (SETQ BEARING (STRCAT DEG "d" MINSS SECS ))

			(IF (or (/= sec "")(/= MINS ""))(SETQ DEG (STRCAT DEG ".")))

  (SETQ LBEARING (STRCAT DEG MINS SEC))

			
    

			);P&IF


(PROGN;ELSE

  

  (SETQ DPOS (vl-string-position 100 BEARING 0))
  (setq Wpos (vl-string-position 39 BEARING 0))
  (setq WWpos (vl-string-position 34 BEARING 0))

    (setq DEG (substr BEARING 1 Dpos))
      (setq MINS (substr BEARING (+ Dpos 2) (- (- WPOS DPOS) 1)))
      (setq SEC (substr BEARING (+ Wpos 2) (- (- WWpos Wpos) 1)))

  (IF (= (STRLEN MINS) 1)(SETQ MINS (STRCAT "0" MINS)))
  (IF (= (STRLEN SEC) 1)(SETQ SEC (STRCAT "0" SEC)))
  
  (IF (AND (= MINS "00") (= SEC "00")) (SETQ MINSS ""
					     SECS ""
					     MINS ""
					     SEC "")
    (SETQ MINSS(STRCAT MINS "'")
	  SECS (STRCAT SEC "\""))
	  	  )
  (IF (= SECS "00\"")(SETQ SECS ""
			   SEC ""))
  
  (SETQ BEARING (STRCAT DEG "d" MINSS SECS ))

  	(IF (or (/= sec "")(/= MINS ""))(SETQ DEG (STRCAT DEG ".")))

  
  ));PELSE&IF


  
    				
 (SETQ CP1 (TRANS  sp 0 1))
    (SETQ CP2 (TRANS  p2 0 1))
    (SETQ ANG (ANGLE CP1 CP2))
    (SETQ DANG (* ANG (/ 180 PI)))

    (IF (AND (> DANG 90) (<= DANG 270)) (PROGN
					 (SETQ CP1 (TRANS p2 0 1))
					 (SETQ CP2 (TRANS sp 0 1))
					 (SETQ ANG (ANGLE CP1 CP2))
                                         (SETQ DANG (* ANG (/ 180 PI)))
					 )
      )
 (SETQ MPE (/ (+ (CAR CP1 ) (CAR CP2)) 2))
    (SETQ MPN (/ (+ (CADR CP1 ) (CADR CP2)) 2))
    (SETQ MP (LIST MPE MPN))
    (SETQ BPOS (POLAR MP (+ ANG (* 0.5 PI)) TH))
    (SETQ DPOS (POLAR MP (- ANG (* 0.5 PI)) TH))
    (SETQ BTS (vl-string-subst  "°" "d" bearing))

 
  (setq prevlayer (getvar "CLAYER"))
   (SETVAR "CLAYER"  "Drafting AFR" )
    (COMMAND "TEXT" "J" "MC" BPOS TH (ANGTOS ANG 1 4) BTS)


  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 chainlist)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
  
  (setvar "clayer" prevlayer)

  )



;------------------------------------------------------------Create Occupation Queensland Style---------------------------------------------

(defun C:XOQ (/)
  (setq prevlayer (getvar "CLAYER"))

  (setq occpnt (getpoint "\nOccupation Point:"))
  (setq bdycnr (getpoint "\nBoundary Corner:"))
  (setq bdyl1 (car (entsel "\nBoundary Line:")))
  (setq desc (getstring T "\nDescription of Occupation:" ))

  (if (= desc "") (setq desc "OCC"))

;check line one
;check offset to line
 
  (SETQ P11 (CDR(ASSOC 10 (ENTGET bdyl1))))
  (SETQ P12 (CDR(ASSOC 11 (ENTGET bdyl1))))
  (SETQ ANG1 (ANGLE P11 P12))
  (SETQ CANG (+ ANG1 (/ PI 2)))
  (SETQ P4 (POLAR occpnt CANG 50))
  (SETQ P6 (POLAR occpnt (+ CANG PI) 50))
   
   (SETQ P5 (INTERS P11 P12 P6 P4 nil))					
 
      (SETQ OFF1 (DISTANCE BDYCNR P5))
      (SETQ OFF2 (DISTANCE P5 OCCPNT))
  (SETQ ANG1 (ANGLE BDYcnr P5))
  (SETQ ANG2 (ANGLE P5 OCCPNT))
  

    


  (IF (= QROUND "YES")
    (progn
      ;ROUND ALL DISTANCES TO 5MM
    (SETQ LIP (FIX (/ OFF1 0.005)))
    (SETQ LFP (- (/ OFF1 0.005) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ OFF1 (* LIP 0.005))
    
    

     (SETQ LIP (FIX (/ OFF2 0.005)))
    (SETQ LFP (- (/ OFF2 0.005) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ OFF2 (* LIP 0.005))
    
    
   
   
    ))



  ;Discover directions
  (if (and (<= ang1 (* (/ 5.0 8.0) pi))(>= ang1 (* (/ 3.0 8.0) pi)))(setq dir1 "N"))
  (if (and (> ang1 (* (/ 1.0 8.0) pi))(< ang1 (* (/ 3.0 8.0) pi)))(setq dir1 "NE"))
  (if (or (>= ang1 (* (/ 15.0 8.0) pi))(<= ang1 (* (/ 1.0 8.0) pi)))(setq dir1 "E"))
  (if (and (> ang1 (* (/ 13.0 8.0) pi))(< ang1 (* (/ 15.0 8.0) pi)))(setq dir1 "SE"))
  (if (and (>= ang1 (* (/ 11.0 8.0) pi))(<= ang1 (* (/ 13.0 8.0) pi)))(setq dir1 "S"))
  (if (and (> ang1 (* (/ 9.0 8.0) pi))(< ang1 (* (/ 11.0 8.0) pi)))(setq dir1 "SW"))
  (if (and (>= ang1 (* (/ 7.0 8.0) pi))(<= ang1 (* (/ 9.0 8.0) pi)))(setq dir1 "W"))
  (if (and (> ang1 (* (/ 5.0 8.0) pi))(< ang1 (* (/ 7.0 8.0) pi)))(setq dir1 "NW"))
  
  

   (if (and (<= ang2 (* (/ 5.0 8.0) pi))(>= ang2 (* (/ 3.0 8.0) pi)))(setq dir2 "N"))
  (if (and (> ang2 (* (/ 1.0 8.0) pi))(< ang2 (* (/ 3.0 8.0) pi)))(setq dir2 "NE"))
  (if (or (>= ang2 (* (/ 15.0 8.0) pi))(<= ang2 (* (/ 1.0 8.0) pi)))(setq dir2 "E"))
  (if (and (> ang2 (* (/ 13.0 8.0) pi))(< ang2 (* (/ 15.0 8.0) pi)))(setq dir2 "SE"))
  (if (and (>= ang2 (* (/ 11.0 8.0) pi))(<= ang2 (* (/ 13.0 8.0) pi)))(setq dir2 "S"))
  (if (and (> ang2 (* (/ 9.0 8.0) pi))(< ang2 (* (/ 11.0 8.0) pi)))(setq dir2 "SW"))
  (if (and (>= ang2 (* (/ 7.0 8.0) pi))(<= ang2 (* (/ 9.0 8.0) pi)))(setq dir2 "W"))
  (if (and (> ang2 (* (/ 5.0 8.0) pi))(< ang2 (* (/ 7.0 8.0) pi)))(setq dir2 "NW"))

  ;calc bdy corner

  
(setvar "clayer" "Occupations")
  (command "point" bdycnr)
  
 (SETQ BDINFO (STRCAT "type=\"Occupation\" state=\"Found\" desc=\"" desc " " (rtos off1 2 3) " " dir1 " " (rtos off2 2 3) " " dir2 "\" />"))
 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
(setvar "clayer" "Drafting")
  (COMMAND "TEXT" "J" "TL" BDYCNR TH (ANGTOS 0 1 4) (strcat desc " " (rtos off1 2 3) " " dir1 " " (rtos off2 2 3) " " dir2))
				
  

 
  (setvar "clayer" prevlayer)


  )



;-----------------------------------------------------------------CREATE ADMIN SHEET-------------------------------
(defun c:XAS (/)

  (SETQ tbinsertpoint (GETPOINT "\n Select Bottom Left Point to Place Admin Sheet:"))
  (setq prevlayer (getvar "clayer"))
  (setvar "clayer" "Admin Sheet")
  

  (setq shname (GetString "\nPlan Number:"))
  (setq shlga (getstring T "\nLGA (* for list): " ))


  (if (= shlga "*")(progn
		      (setq workingselnum "0")
		      (setq names lgalist)
		      (dbox)
		      (setq parishnumber returntype)
		      
		      (setq ,pos1 (vl-string-position 44 parishnumber 0))
                 (setq shlga  (substr parishnumber 1 ,pos1))
      		 (setq shlgacode  (substr parishnumber (+ ,pos1 2) 50))
		      
		      )
  
  (setq shlgacode (getstring T "\nLGA Code:" ))
    )

  
  (setq shParish (getstring T "\nParish (* for list):"))
  (if (= shparish "*")(progn
		      (setq workingselnum "0")
		      (setq names parishlist)
		      (dbox)
		      (setq parishnumber returntype)
		      
		      (setq ,pos1 (vl-string-position 44 parishnumber 0))
                 (setq shparish  (substr parishnumber 1 ,pos1))
      		 (setq shParishcode  (substr parishnumber (+ ,pos1 2) 50))
		      
		      )
    
  (setq shParishcode (getstring T "\nParish Code:"))
    )


  (setq shfirm (getstring T "\nFirm/Address:" ))
  (setq shref (getstring T "\nSurveyors Reference and Version (####-Ver1):" ))

 
 (setq shts (getstring T "\nTownship (* for list):"))
  (if (= shts "*")(progn
		      (setq workingselnum "0")
		      (setq names townlist)
		      (dbox)
		      (setq parishnumber returntype)
		      
		      (setq ,pos1 (vl-string-position 44 parishnumber 0))
                 (setq shts  (substr parishnumber 1 ,pos1))
      		 (setq shtscode  (substr parishnumber (+ ,pos1 2) 50))
		      
		      )
         )

  
  (setq shcs (getstring T "\nCrown Section:" ))
  (setq shca (getstring T "\nCrown Alloment:" ))
  (setq shcp (getstring T "\nCrown Portion:" ))
  (setq exttitles (getstring T "\nTitle References:" ))
  (setq extaddress (getstring T "\nExtinguished Lot Addresses:" ))
  (setq shlpr (getstring T "\nLast plan reference:" ))
  (setq shdl (getstring T "\nDepth Limitation:" ))
     (setq shtype (getstring T "\nSurveyed, Compiled or Partially Surveyed (S/C/P):" ))
  (if (or (= shtype "S")(= shtype "s")(= shtype "SURVEYED")(= shtype "Surveyed"))(setq shtype "surveyed"))
  (if (or (= shtype "C")(= shtype "c")(= shtype "COMPILED")(= shtype "Compiled"))(setq shtype "compiled"))
  (if (or (= shtype "P")(= shtype "P")(= shtype "PARTIALLY SURVEYED")(= shtype "partially Surveyed"))(setq shtype "partially surveyed"))
  (setq shpurpose (getstring T "\nPlan of (* for list):" ))
  (if (= shpurpose "*")(progn
		      (setq workingselnum "0")
		      (setq names purposelist)
		      (dbox)
		      (setq shpurpose returntype)
		      
		      (setq -pos1 (vl-string-position 45 shpurpose 0))
                 (setq shpurpose1  (substr shpurpose 1 -pos1))
      		 (setq shpurpose2  (substr shpurpose (+ -pos1 2) ))
		      
		      )
         )
  (if (= shpurpose2 nil)(setq shpurpose2 ""))
  (if (= shpurpose1 nil)(setq shpurpose1 ""))
  
    
      (setq shhop (getstring T "\nSub Act(S)/Owners Corp(O)/Trasfer Land Act(T):" ))
  (if (<= (strlen shhop) 3)(setq shhop (strcase shhop)))
  (if (= shhop "S")(setq shhop "Subdivision Act 1988"))
  (if (= shhop "O")(setq shhop "Owners Corporation Act 2006"))
  (if (= shhop "T")(setq shhop "Transfer of Land Act 1958"))
  (if (or (= shhop "TO")(= shhop "OT"))(setq shhop "Transfer of Land Act 1958 & Owners Corporation Act 2006"))
  (if (or (= shhop "SO")(= shhop "OS"))(setq shhop "Subdivision Act 1988 & Owners Corporation Act 2006"))
  (if (or (= shhop "SOT")
	  (= shhop "STO")
	  (= shhop "OST")
	  (= shhop "OTS")
	  (= shhop "TOS")
	  (= shhop "TSO"))(setq shhop "Subdivision Act 1988 & Owners Corporation Act 2006 & Transfer of Land Act 1958"))
      
(setq shsurveyor (getstring T "\nSurveyor:" ))
  (setq shregnumber (getstring T "\nSurveyors Reg Number:" ))
  (setq shdos (getstring T "\nDate of Survey:" ))
  
  (if (/= shdos "")(progn

;sort date entrys

  ;replace /,\,. with -
  (setq /pos 0)
	      (while (/=  (setq /pos (vl-string-search "/" shdos /pos )) nil) (setq shdos (vl-string-subst "-" "/"  shdos /pos)
										      /pos (+ /pos 1)))
(setq /pos 0)
	      (while (/=  (setq /pos (vl-string-search "\\" shdos /pos )) nil) (setq shdos (vl-string-subst "-" "\\"  shdos /pos)
										      /pos (+ /pos 2)))
  (setq /pos 0)
	      (while (/=  (setq /pos (vl-string-search "." shdos /pos )) nil) (setq shdos (vl-string-subst "-" "."  shdos /pos)
									      /pos (+ /pos 1)))
   ));p&if dos not ""

  (setq shjur (getstring T "\nJurisdiction (VIC is default):" ))
  (if (= shjur "")(setq shjur "Victoria"))

  (setq shformat (getstring T "\nsTerrain (Level(L)/Undulating(U)/Steep(S)/Mountainous(M):" ))
  (if (or (= shformat "L")(= shformat "l")(= (strcase shformat) "LEVEL")(= shformat "level"))(setq shformat "Level Land"))
  (if (or (= shformat "U")(= shformat "u")(= (strcase shformat) "UNDULATING"))(setq shformat "Undulating Land"))
  (if (or (= shformat "S")(= shformat "s")(= (strcase shformat) "STEEP"))(setq shformat "Steep Land"))
  (if (or (= shformat "M")(= shformat "m")(= (strcase shformat) "MOUNTAINOUS"))(setq shformat "Mountainous Land"))

  

  (setq datum (getstring T "\nAzimuth Datum (MGA_Zone55,Local etc (* for list):" ))
   (if (= datum "*")
    (progn
		      (setq workingselnum "2")
		      (setq names datumlist)
		      (dbox)
		      (setq datum returntype)
		       ));p&if format*


  (setq hdatum (getstring T "\nHorizontal Datum (MGA94_Zone55,Local etc (* for list):" ))
   (if (= hdatum "*")
    (progn
		      (setq workingselnum "2")
		      (setq names hdatumlist)
		      (dbox)
		      (setq hdatum returntype)
		       ));p&if format*

  (if (= hdatum "Local")
    (progn
      (setq datumdesc  (getstring T "\nLocal datum requires descripton (eg oriented to LP1234):"))
      (if (/= datumdesc "")(setq hdatum (strcat hdatum "~" datumdesc)))
      )
    )
    
				       
  
  (setq zone (getstring T "\nZone (54/55):" ))
  
  (if (= exttitles "")(setq exttitles " "))
  (if (= extaddress "")(setq extaddress " "))
  
  ;changed for BricsCAD
    (SETVAR "clayer" "Admin Sheet")
 (COMMAND "._INSERT" "ADMINSHEET" "_S" "1" tbinsertpoint "0" shname shlga shparish shfirm shref shts shcs shca shcp exttitles "" extaddress "" shlpr shdl shtype shpurpose2 shhop shsurveyor shregnumber shdos shjur shformat datum hdatum zone shlgacode shparishcode shpurpose1)
   	  (setvar "clayer" prevlayer)
  );DEFUN




;----------------------------------------------------------------CREATE LAYOUT------------------------------------------------

(DEFUN C:XLA (/)
(setq prevlayer (getvar "clayer"))
  (setvar "clayer" "Drafting")

  (SETQ SHEET (GETSTRING "Sheet type (* for list):"))

   (if (= sheet "*")(progn
		      (setq workingselnum "0")
		      (setq names laylist)
		      (dbox)
		      (setq sheet returntype)
		      
		      
		      ))

     (if (or (= sheet "2")(= sheet "2.Generic Plan portrait" ))(setq sheet "2GPP"))
     (if (or (= sheet "3")(= sheet "3.Generic Plan-landscape" ))(setq sheet "3GPL"))
     (if (or (= sheet "4")(= sheet "4.Abstract of Field Records-front" ))(setq sheet "4AFRF"))
     (if (or (= sheet "5")(= sheet "5.Abstract of Field Records-subsequent" ))(setq sheet "5AFRS"))
     (if (or (= sheet "6")(= sheet "6.Other Plan-front" ))(setq sheet "6OPF"))
     

	    
     
  
(IF (/= (setq adminsheet (ssget "_X" '((0 . "INSERT") (2 . "ADMINSHEET")))) nil)(progn
		(SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME ADMINSHEET 0)))))
		(SETQ INSP (CDR(ASSOC 10 (ENTGET (SSNAME ADMINSHEET 0)))))

(setq count 1)
  (SETQ ATTLIST (LIST))
	    
(setq Obj (vlax-ename->vla-object En))
	(foreach x (vlax-invoke Obj 'GetAttributes)
	  (SETQ ATT (vla-get-textstring X))
	  (if (= att "none") (setq att ""))

	  
	  ;(setq crlfpos 0)
	  ; (while (/=  (setq crlfpos (vl-string-search "\p" att crlfpos )) nil) (setq att (vl-string-subst "&#xA;" "\p"  att crlfpos)
	;									      crlfpos (+ crlfpos 5)))


	  (setq attlist (append attlist (list att)))

      
	  )

	

		(if (or (= sheet "3GPL")(= sheet "2GPP")(= sheet "6OPF"))(setvar "clayer" "Drafting"))
		(if (or (= sheet "4AFRF")(= sheet "5AFRS"))(setvar "clayer" "Drafting AFR")) 
		

		;changed for BricsCAD
		(IF (= SHEET "3GPL")  (COMMAND "._INSERT" "3GPL" "_S" (/ SCALE 1000.0) INSP "0" (NTH 16 ATTLIST) ))
		(IF (= SHEET "2GPP")  (COMMAND "._INSERT" "2GPP" "_S" (/ SCALE 1000.0) INSP "0" (NTH 16 ATTLIST) ))
		(IF (= SHEET "6OPF")  (COMMAND "._INSERT" "6OPF" "_S" (/ SCALE 1000.0) INSP "0" (NTH 2 ATTLIST)(NTH 4 ATTLIST)(NTH 5 ATTLIST)(NTH 6 ATTLIST)(NTH 7 ATTLIST)(NTH 8 ATTLIST)(NTH 11 ATTLIST)(NTH 9 ATTLIST)(NTH 16 ATTLIST) ))
		(IF (= SHEET "4AFRF")  (COMMAND "._INSERT" "4AFRF" "_S" (/ SCALE 1000.0) INSP "0" (NTH 4 ATTLIST) ))
		(IF (= SHEET "5AFRS")  (COMMAND "._INSERT" "5AFRS" "_S" (/ SCALE 1000.0) INSP "0" (NTH 4 ATTLIST) ))
		
		

		  
		
  )
  (progn
    (SETQ P1 (GETPOINT "\n Select Bottom Right Point to Place Layout Sheet:"))

    
  (COMMAND "._INSERT" sheet "_S" (/ SCALE 1000.0) p1 "0");changed for BricsCAD

    
    ));P&IF ADMINSHEET EXISTS
  (setvar "clayer" prevlayer)
);defun


;--------------------------------------------------------------Autostn--------------------------------------


(defun C:XCS (/)

  (SETQ CGPL (LIST));EMPTY CGPL

  (setq prevlayer (getvar "clayer"))
  (setvar "clayer" "Drafting AFR")
  (setq pcount 1)
  

(IF (/= (setq lots (ssget "_X" '((0 . "Line") (8 . "Traverse")))) nil)(progn

										     
 (setq count 0)
  (repeat (sslength lots)
  
(SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME LOTS COUNT)))))
 (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME LOTS COUNT)))))

    

   (setq p1s (strcat (rtos (cadr p1) 2 6) " " (rtos (car p1) 2 6)))
   (setq p2s (strcat (rtos (cadr p2) 2 6) " " (rtos (car p2) 2 6)))
  
   ;get edge names
     (if (/= (setq remlist (member p1s cgpl)) nil)(progn
						  (setq p1n (nth 3 remlist))
						  )
       (progn
	
	 (setq pcount (+ pcount 1))
	 (setq cgpl (append cgpl (list p1s)(list "sideshot")(list "proposed")(list (rtos pcount 2 0))))
         (setq p1n (rtos pcount 2 0))
	 
	 	 ))
     (if (/= (setq remlist (member p2s cgpl)) nil)(progn
						  (setq p2n (nth 3 remlist))
						  )
       (progn;if not found at corner
	 
	 (setq pcount (+ pcount 1))
	 (setq cgpl (append cgpl (list p2s)(list "sideshot")(list "proposed")(list (rtos pcount 2 0))))
         (setq p2n (rtos pcount 2 0))
	 
	 	 ))
    

 
  (setq count (+ count 1))
);r length of lots
										    



  (setq count 0)
  (repeat (/ (length cgpl ) 4)
    (setq linetext (nth count cgpl))

(setq spcpos (vl-string-position 32 linetext ))
(setq north (atof (substr linetext 1 (+ spcpos 2 ))))
(setq east (atof (substr linetext (+ spcpos 2) 50)))
    (setq p1 (list east north))
    
    (COMMAND "._INSERT" "VSTN" "_S" TH (trans p1 0 1) "0");changed for BricsCAD
    (setq count (+ count 4))
    );repeat

  (setvar "clayer" prevlayer)

 ));p&if lots found
)




;--------------------------------------------------------------Autotick--------------------------------------


(defun C:XMT (/)

  (SETQ CGPL (LIST));EMPTY CGPL

  (setq prevlayer (getvar "clayer"))
  (setvar "clayer" "Drafting")
  (setq pcount 1)

  (setq tickpossting (list))
  (setq tickposlist (list))
  (setq tickrot (list))
  

(IF (/= (setq lots (ssget  '((0 . "LWPOLYLINE") (8 . "Lot Definitions")))) nil)(progn

										     
 (setq count 0)
  (repeat (sslength lots)
  
    (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))
    

    
  

					(setq enlist (entget en))
    ;go through polyline to get points and bugle factors
    (SETQ PTLIST (LIST))
	    (foreach a enlist
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	      
	    )				;FOREACH 			
 
(setq ptlist (append ptlist (list(nth 0 ptlist))))
(setq ptlist (append ptlist (list(nth 1 ptlist))))
    
 (setq count1 0)
 (repeat (-  (length ptlist ) 2)

   (setq p1 (nth count1 ptlist))
   (setq p2 (nth (+ count1 1) ptlist))
   (setq p3 (nth (+ count1 2) ptlist))
   (setq ang1 (angle p2 p1))
   (setq ang2 (angle p2 p3))
   
   (setq dang1 (- ang2 ang1))
   (if (< dang1 0)(setq dang1 (+ dang1 (* 2 pi))))
   (setq dang2 (- ang1 ang2))
   (if (< dang2 0)(setq dang2 (+ dang2 (* 2 pi))))
   (if (< dang2 dang1) (setq dang (- pi dang2)))
   (if (< dang1 dang2) (setq dang (- pi dang1)))
      
   (princ (strcat "\n" (rtos dang 2 5)))
   
   (if (and (< dang (* 0.11111 pi)) (/= member (strcat (rtos (car p2) 2 6) "," (rtos (cadr p2) 2 6)) tickpos))
     (progn
       (setq tickposstring (append tickpostring (list (strcat (rtos (car p2) 2 6) "," (rtos (cadr p2) 2 6)))))
       (setq tickposlist (append tickposlist (list p2)))
       (setq tickrot (append tickrot (list (+ ang1 (/ (- ang2 ang1) 2)))))
       ))

   
	 	 
    
  (setq count1 (+ count1 1))

   );r length of ptlist
					
 
  (setq count (+ count 1))
);r length of lots
										    



  (setq count 0)
  (repeat  (length tickposlist ) 
    (setq pos (nth count tickposlist))
    (setq rot (nth count tickrot))

    (setq tp1 (Trans (polar pos rot (* 0.25 th)) 0 1))
    (setq tp2 (trans (polar pos (+ rot pi)(* 0.25 th)) 0 1))
    (command "line" tp1 tp2 "")
    

    (setq count (+ count 1))
    );repeat

  (setvar "clayer" prevlayer)

 ));p&if lots found
)




;-------------------------------------------------------------ASSIGN LINE TO XML-----------------------------------
;reworked to include XALN

(defun C:XAL (/)
  (setq notereq 0)
  (al)
  )
(defun C:XALN (/)
  (setq notereq 1)
  (al)
  )

(defun C:XALL (/)
  (setq notereq 2)
  (al)
  )


(DEFUN AL (/)

  (setq XALprevlayer (getvar "CLAYER"))
(SETQ COMMENT "")
 (SETQ LINES (SSGET  '((0 . "LINE"))))

  (SETQ COUNT 0)
(REPEAT (SSLENGTH LINES)
(SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ P1 (LIST (CAR P1) (CADR P1)));2DISE P1 TO GIVE 2D DISTANCE
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ LAYER (CDR(ASSOC 8 (ENTGET (SSNAME LINES COUNT)))))

  (SETQ ANG (ANGLE  P1  P2 ))

  (SETQ BEARING (ANGTOS ANG 1 4));REQUIRED FOR ELSE ROUND
  (setq bearing (vl-string-subst "d" (chr 176) bearing));added for BricsCAD changes degrees to "d"
  (SETQ DIST (DISTANCE (LIST (CAR P1)(CADR P1)) P2));REQUIRED FOR ELSE ROUND


  

  (IF (= QROUND "YES")(PROGN
  
 	(SETQ LLEN (DISTANCE (LIST (CAR P1)(CADR P1)) P2))

    ;ASSIGN ROUNDING FOR ANGLES BASED ON DISTANCE
    (IF (< LLEN MAXLEN1) (SETQ ROUND BRND1))
    (IF (AND (> LLEN MAXLEN1)(< LLEN MAXLEN2)) (SETQ ROUND BRND2))
    (IF (> LLEN MAXLEN2)(SETQ ROUND BRND3))			
			
    ;(IF (> LLEN 100) (SETQ ROUND 1))

   
    ;GET ANGLE DELIMIETERS
    (SETQ SANG (ANGTOS ANG 1 4))
    (setq sang (vl-string-subst "d" (chr 176) sang));added for BricsCAD changes degrees to "d"
    (setq CHRDPOS (vl-string-position 100 SANG 0))
    (setq MINPOS (vl-string-position 39 SANG 0))
    (setq SECPOS (vl-string-position 34 SANG 0))

    ;PARSE ANGLE
    (setq DEG  (atof (substr SANG 1  CHRDPOS )))
    (setq MINS  (atof (substr SANG (+ CHRDPOS 2)  (-(- MINPOS CHRDPOS)1))))
    (setq SEC  (atof (substr SANG (+ MINPOS 2)  (-(- SECPOS MINPOS )1))))

   
;ROUND ANGLE, NOTE SECONDS REMOVED
    (IF (and (= ROUND 60)(< SEC 30)) (SETQ SEC 0))
    (IF (and (= ROUND 60)(>= SEC 30)) (SETQ SEC 0
					    MINS (+ MINS 1)))					  
    (IF (/= ROUND 60) (PROGN
			(SETQ SIP (FIX (/ SEC ROUND)))
			(SETQ SFP (- (/  SEC ROUND) SIP))
			(IF (>= SFP 0.5) (SETQ SIP (+ SIP 1)))
			(SETQ SEC (* SIP ROUND))
			)
      )

    ;ROUND DISTANCES

    (IF (< LLEN DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> LLEN DISTMAX1)(< LLEN DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> LLEN DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ LLEN DROUND)))
    (SETQ LFP (- (/ LLEN DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ LLEN (* LIP DROUND))

    (SETQ LDIST (RTOS LLEN 2 3))
    
    ;(IF (< LLEN 1) (SETQ DTS (STRCAT "0" DTS)))
      
   
		 
    
;STRING ANGLES
    (SETQ DEG (RTOS DEG 2 0))
    (SETQ MINS (RTOS MINS 2 0))
    (SETQ SEC (RTOS SEC 2 0))
    
;INCREMENT IF SECONDS ROUNDED TO 60
    (IF (= SEC  "60")
      (PROGN
	(SETQ SEC "00")
	(SETQ MINS (RTOS (+ (ATOF MINS ) 1) 2 0))
	)
      )
;INCREMENT IF MINUTES ROUNDED TO 60
    (IF (= MINS "60")
      (PROGN
	(SETQ MINS "00")
	(SETQ DEG (RTOS (+ (ATOF DEG ) 1) 2 0))
	)
      )
;FIX IF INCREMENTING PUSHES DEG PAST 360    
    (IF (= DEG "360")(SETQ DEG "0"))
;ADD ZEROS TO SINGLE NUMBERS	
 (IF (= (STRLEN MINS) 1)(SETQ MINS (STRCAT "0" MINS)))
  (IF (= (STRLEN SEC) 1)(SETQ SEC (STRCAT "0" SEC)))

;TRUNCATE BEARINGS IF 00'S
  (IF (AND (= MINS "00") (= SEC "00")) (SETQ MINSS ""
					     MINS ""
					     SECS ""
					     SEC "")
        (SETQ MINSS(STRCAT MINS "'")
	  SECS (STRCAT SEC "\""))
	  )
    (IF (= SEC "00")(SETQ SECS ""
			    SEC ""))

			

    ;CONCATENATE BEARING
    (SETQ BEARING (STRCAT DEG "d" MINSS SECS ))

			(IF (or (/= sec "")(/= MINS ""))(SETQ DEG (STRCAT DEG ".")))

  (SETQ LBEARING (STRCAT DEG MINS SEC))

			
    

			);P&IF


(PROGN;ELSE

  

  (SETQ DPOS (vl-string-position 100 BEARING 0))
  (setq Wpos (vl-string-position 39 BEARING 0))
  (setq WWpos (vl-string-position 34 BEARING 0))

    (setq DEG (substr BEARING 1 Dpos))
      (setq MINS (substr BEARING (+ Dpos 2) (- (- WPOS DPOS) 1)))
      (setq SEC (substr BEARING (+ Wpos 2) (- (- WWpos Wpos) 1)))

  (IF (= (STRLEN MINS) 1)(SETQ MINS (STRCAT "0" MINS)))
  (IF (= (STRLEN SEC) 1)(SETQ SEC (STRCAT "0" SEC)))
  
  (IF (AND (= MINS "00") (= SEC "00")) (SETQ MINSS ""
					     SECS ""
					     MINS ""
					     SEC "")
    (SETQ MINSS(STRCAT MINS "'")
	  SECS (STRCAT SEC "\""))
	  	  )
  (IF (= SECS "00\"")(SETQ SECS ""
			   SEC ""))
  
  (SETQ BEARING (STRCAT DEG "d" MINSS SECS ))

  	(IF (or (/= sec "")(/= MINS ""))(SETQ DEG (STRCAT DEG ".")))

  
  
  (SETQ LBEARING (STRCAT DEG MINS SEC))
  (SETQ LDIST (RTOS DIST 2 3))

  ));PELSE&IF

  (if ( = notereq 1) (setq comment ( getstring T "\nGeometry Note:")))
 

  
  (COMMAND "ERASE" EN "")
  (SETVAR "CLAYER" layer)
  (COMMAND "LINE" (trans P1 0 1)(trans P2 0 1) "")

    ;LOOK FOR SPECIAL COMMENTS
  		     ;LOOK FOR SPECIAL COMMENTS
  (setq distancetype "" azimuthtype "")
  (IF (OR (= (STRCASE comment) "AD")(= (STRCASE comment) "ADOPT"))(setq distancetype " distanceType=\"Adopt Dimension\" " azimuthtype " azimuthType=\"Adopt Dimension\" " comment ""))
  (if (= (strcase (substr comment 1 3)) "AD ")(setq distancetype " distanceType=\"Adopt Dimension\" " azimuthtype " azimuthType=\"Adopt Dimension\" " ))
  (IF (OR (= (STRCASE comment) "COMP")(= (STRCASE comment) "COMPUTED"))(setq distancetype " distanceType=\"Computed\" " azimuthtype " azimuthType=\"Computed\" "  comment ""))
  (if (= (strcase (substr comment 1 5)) "COMP ")(setq distancetype " distanceType=\"Computed\" " azimuthtype " azimuthType=\"Computed\" " ))
  (IF (OR (= (STRCASE comment) "D")(= (STRCASE comment) "DERIVED"))(setq distancetype " distanceType=\"Derived\" " azimuthtype " azimuthType=\"Derived\" "  comment ""))
  (if (= (strcase (substr comment 1 8)) "DERIVED ")(setq distancetype " distanceType=\"Derived\" " azimuthtype " azimuthType=\"Derived\" " ))
  (IF (OR (= (STRCASE comment) "MEAS")(= (STRCASE comment) "MEASURED"))(setq distancetype " distanceType=\"Measured\" " azimuthtype " azimuthType=\"Measured\" "  comment ""))
  (if (= (strcase (substr comment 1 9)) "MEASURED ")(setq distancetype " distanceType=\"Measured\" " azimuthtype " azimuthType=\"Measured\" " ))
  
 (if (/= comment "")(setq ocomment (strcat "><FieldNote>\"" comment "\"</FieldNote></ReducedObservation>"))(setq ocomment "/>"))
  
(SETQ BDINFO (STRCAT "azimuth=\"" lbearing "\" horizDistance=\"" ldist "\"" azimuthtype distancetype  ocomment))
  (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)


  (LBA)

  (SETQ COUNT (+ COUNT 1))

  (IF (= notereq 2) (command "leader" p1 p2 "" "" "N" "leader" p2 p1 "" "" "N")) 

  );R


      (SETVAR "CLAYER" XALprevlayer)

  );DEFUN
  


;-------------------------------------------------------------ASSIGN ARC TO XML-----------------------------------
(defun C:XAA (/)
  (setq notereq 0)
  (AA)
  )
(defun C:XAAN (/)
  (setq notereq 1)
  (AA)
  )

(DEFUN AA (/)



  (setq XALprevlayer (getvar "CLAYER"))
(SETQ COMMENT "")
 (SETQ LINES (SSGET  '((0 . "ARC"))))

  (SETQ COUNT 0)
(REPEAT (SSLENGTH LINES)
(SETQ CP (CDR(ASSOC 10 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ RADIUS (CDR(ASSOC 40 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ LAYER (CDR(ASSOC 8 (ENTGET (SSNAME LINES COUNT)))))

  (SETQ ANG1 (CDR(ASSOC 50 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ ANG2 (CDR(ASSOC 51 (ENTGET (SSNAME LINES COUNT)))))

  (SETQ P1 (POLAR CP ANG1 RADIUS))
  (SETQ P2 (POLAR CP ANG2 RADIUS))
  (SETQ ANG (ANGLE P1 P2))
  
(SETQ CURVEROT "ccw")
  ;calc curve midpoint
  (setq a1 (angle CP p1))
  (setq a2 (angle CP p2))
  (if (= curverot "ccw")(setq da (- a2 a1))(setq da (- a1 a2)))
  (if (< da 0)(setq da (+ da (* 2 pi))))
    (SETQ DA (/ DA 2))
    (IF (= CURVEROT "ccw")(setq midb (+ a1 da))(setq midb (+ a2 da)))
  (setq amp (polar CP midb radius))

  (SETQ BEARING (ANGTOS ANG 1 4))
  (setq bearing (vl-string-subst "d" (chr 176) bearing));added for BricsCAD changes degrees to "d"
  (SETQ DIST (DISTANCE (LIST (CAR P1)(CADR P1))P2))

(IF (= QROUND "YES")(PROGN

			(SETQ LLEN (DISTANCE (LIST (CAR P1)(CADR P1)) P2))

    ;ASSIGN ROUNDING FOR ANGLES BASED ON DISTANCE
      (IF (< LLEN MAXLEN1) (SETQ ROUND BRND1))
    (IF (AND (> LLEN MAXLEN1)(< LLEN MAXLEN2)) (SETQ ROUND BRND2))
    (IF (> LLEN MAXLEN2)(SETQ ROUND BRND3))

   
    ;GET ANGLE DELIMIETERS
    (SETQ SANG (ANGTOS ANG 1 4))
    (setq sang (vl-string-subst "d" (chr 176) sang));added for BricsCAD changes degrees to "d"
    (setq CHRDPOS (vl-string-position 100 SANG 0))
    (setq MINPOS (vl-string-position 39 SANG 0))
    (setq SECPOS (vl-string-position 34 SANG 0))

    ;PARSE ANGLE
    (setq DEG  (atof (substr SANG 1  CHRDPOS )))
    (setq MINS  (atof (substr SANG (+ CHRDPOS 2)  (-(- MINPOS CHRDPOS)1))))
    (setq SEC  (atof (substr SANG (+ MINPOS 2)  (-(- SECPOS MINPOS )1))))

   
;ROUND ANGLE, NOTE SECONDS REMOVED
     (IF (and (= ROUND 60)(< SEC 30)) (SETQ SEC 0))
    (IF (and (= ROUND 60)(>= SEC 30)) (SETQ SEC 0
					    MINS (+ MINS 1)))	
		       
    (IF (/= ROUND 60) (PROGN
			(SETQ SIP (FIX (/ SEC ROUND)))
			(SETQ SFP (- (/  SEC ROUND) SIP))
			(IF (>= SFP 0.5) (SETQ SIP (+ SIP 1)))
			(SETQ SEC (* SIP ROUND))
			)
      )

		      
    (IF (< LLEN DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> LLEN DISTMAX1)(< LLEN DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> LLEN DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ LLEN DROUND)))
    (SETQ LFP (- (/ LLEN DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ LLEN (* LIP DROUND))
    
    (SETQ LDIST (RTOS LLEN 2 3))
    ;(IF (< LLEN 1) (SETQ DTS (STRCAT "0" DTS)))
      
   
		 
    
;STRING ANGLES
    (SETQ DEG (RTOS DEG 2 0))
    (SETQ MINS (RTOS MINS 2 0))
    (SETQ SEC (RTOS SEC 2 0))
    
;INCREMENT IF SECONDS ROUNDED TO 60
    (IF (= SEC  "60")
      (PROGN
	(SETQ SEC "00")
	(SETQ MINS (RTOS (+ (ATOF MINS ) 1) 2 0))
	)
      )
;INCREMENT IF MINUTES ROUNDED TO 60
    (IF (= MINS "60")
      (PROGN
	(SETQ MINS "00")
	(SETQ DEG (RTOS (+ (ATOF DEG ) 1) 2 0))
	)
      )
;FIX IF INCREMENTING PUSHES DEG PAST 360    
    (IF (= DEG "360")(SETQ DEG "0"))
;ADD ZEROS TO SINGLE NUMBERS	
 (IF (= (STRLEN MINS) 1)(SETQ MINS (STRCAT "0" MINS)))
  (IF (= (STRLEN SEC) 1)(SETQ SEC (STRCAT "0" SEC)))

;TRUNCATE BEARINGS IF 00'S
  (IF (AND (= MINS "00") (= SEC "00")) (SETQ MINSS ""
					     MINS ""
					     SECS ""
					     SEC "")
    ;ELSE
        (SETQ MINSS(STRCAT MINS "'")
	  SECS (STRCAT SEC "\""))
	  )
    (IF (= SECS "00\"")(SETQ SECS ""
			     SEC ""))

			

    ;CONCATENATE BEARING
    (SETQ BEARING (STRCAT DEG "d" MINSS SECS ))

			(IF (or (/= sec "")(/= MINS ""))(SETQ DEG (STRCAT DEG ".")))

  (SETQ LBEARING (STRCAT DEG MINS SEC))
			
    

			);P&IF ROUNDING


(PROGN;ELSE

  

  (SETQ DPOS (vl-string-position 100 BEARING 0))
  (setq Wpos (vl-string-position 39 BEARING 0))
  (setq WWpos (vl-string-position 34 BEARING 0))

    (setq DEG (substr BEARING 1 Dpos))
      (setq MINS (substr BEARING (+ Dpos 2) (- (- WPOS DPOS) 1)))
      (setq SEC (substr BEARING (+ Wpos 2) (- (- WWpos Wpos) 1)))

  (IF (= (STRLEN MINS) 1)(SETQ MINS (STRCAT "0" MINS)))
  (IF (= (STRLEN SEC) 1)(SETQ SEC (STRCAT "0" SEC)))
  
  (IF (AND (= MINS "00") (= SEC "00")) (SETQ MINSS ""
					     SECS ""
					     MINS ""
					     SEC "")
    (SETQ MINSS(STRCAT MINS "'")
	  SECS (STRCAT SEC "\""))
	  )
  (IF (= SECS "00\"")(SETQ SECS ""
			   SEC ""))
  
  (SETQ BEARING (STRCAT DEG "d" MINSS SECS ))

  	(IF (or (/= sec "")(/= MINS ""))(SETQ DEG (STRCAT DEG ".")))

  (SETQ LBEARING (STRCAT DEG MINS SEC))
  (SETQ LDIST (RTOS DIST 2 3))

  ));PELSE&IF




   (SETQ MAST (SQRT (- (*  RADIUS  RADIUS) (* (/  DIST 2)(/ DIST 2 )))))





  
  ;(SETQ O (* 2 (ATAN (/ (/  DIST 2) MAST))))
  (SETQ O (- ANG2 ANG1))
  (IF (< O 0) (SETQ O (+ O (* PI 2))))
  	   (setq arclength (rtos ( *  radius O) 2 3))



  
  (setq digchaz (angle p1 p2))
    (SETQ O1 (* 2 (ATAN (/ (/ (distance p1 p2) 2) MAST))))
	    (setq remhalfO  (- (* 0.5 pi) (/ O1 2)))
	    (if (and (= curverot "ccw") (<= (atof arclength) (* pi  radius)))(setq raybearing (+  digchaz  remhalfO)))
	    (IF (and (= curverot "cw") (<= (atof arclength) (* pi  radius)))(setq raybearing (-  digchaz  remhalfO)))
	    (IF (and (= curverot "ccw") (> (atof arclength) (* pi  radius)))(setq raybearing (-  digchaz  remhalfO)))
	    (if (and (= curverot "cw") (> (atof arclength) (* pi  radius)))(setq raybearing (+  digchaz  remhalfO)))

  

  (if (= qround "YES")(progn 
	(SETQ LLEN (atof arclength))

  (IF (< LLEN DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> LLEN DISTMAX1)(< LLEN DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> LLEN DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ LLEN DROUND)))
    (SETQ LFP (- (/ LLEN DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ LLEN (* LIP DROUND))
    
    (SETQ arclength (RTOS LLEN 2 3))
    ;(IF (< LLEN 1) (SETQ DTS (STRCAT "0" DTS)))
 
  			
	(SETQ LLEN  radius)

  (IF (< LLEN DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> LLEN DISTMAX1)(< LLEN DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> LLEN DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ LLEN DROUND)))
    (SETQ LFP (- (/ LLEN DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ LLEN (* LIP DROUND))
    
    (SETQ radius llen)
    ;(IF (< LLEN 1) (SETQ DTS (STRCAT "0" DTS)))
    
 ))
  


  

  (COMMAND "ERASE" EN "")
  (SETVAR "CLAYER" layer)
  (COMMAND "ARC" "c" (TRANS CP 0 1) (TRANS P1 0 1) (TRANS P2 0 1 ))

  (if ( = notereq 1) (setq comment ( getstring T "\nGeometry Note:")))

    ;LOOK FOR SPECIAL COMMENTS
  (setq arctype "" )
  (IF (OR (= (STRCASE comment) "AD")(= (STRCASE comment) "ADOPT"))(setq arctype " arcType=\"Adopt Dimension\" "  comment ""))
  (if (= (strcase (substr comment 1 3)) "AD ")(setq arctype " arcType=\"Adopt Dimension\" "))
  (IF (OR (= (STRCASE comment) "COMP")(= (STRCASE comment) "COMPUTED"))(setq arctype " arcType=\"Computed\" "   comment ""))
  (if (= (strcase (substr comment 1 5)) "COMP ")(setq arctype " arcType=\"Computed\" "  ))
  (IF (OR (= (STRCASE comment) "D")(= (STRCASE comment) "DERIVED"))(setq arctype " arcType=\"Derived\" "  comment ""))
  (if (= (strcase (substr comment 1 8)) "DERIVED ")(setq arctype " arcType=\"Derived\" " ))
  (IF (OR (= (STRCASE comment) "MEAS")(= (STRCASE comment) "MEASURED"))(setq arctype " arcType=\"Measured\" "   comment ""))
  (if (= (strcase (substr comment 1 9)) "MEASURED ")(setq arctype " arcType=\"Measured\" " ))

  
      (if (/= comment "")(setq ocomment (strcat "><FieldNote>\"" comment "\"</FieldNote></ReducedArcObservation>"))(setq ocomment "/>"))
(SETQ BDINFO (STRCAT "chordAzimuth=\"" Lbearing "\" length=\"" arclength "\" radius=\"" (RTOS RADIUS 2 3)  "\" rot=\"ccw\"" arctype ocomment))
 
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

      (setq tp1 p1)
      (setq tp2 p2)
   
      (setq lradius (rtos radius 2 3))
(lbarc);label line if not already labelled;label arc using function



  (SETQ COUNT (+ COUNT 1))

  );R


      (SETVAR "CLAYER" XALprevlayer)

  );DEFUN
  


;-------------------------------------------------------------ASSIGN POLYLINE TO XML-----------------------------------

(DEFUN C:XAP (/)

  
  (setq prevlayer (getvar "CLAYER"))
(setq areapercent nil)
  (setq calccen "N")
  (if (= plotno nil) (setq plotno "1"))

 (SETQ SENT (CAR (ENTSEL "\nSelect Polyline:")))
 (setq lotc (getpoint "\nSelect Lot Centre (default Centroid):"))
    
  (SETQ SENTLIST (ENTGET SENT))
    ;go through polyline to get points to check for clockwise direction
  (SETQ ZA (CDR (ASSOC 210 SENTLIST)))
    (SETQ CWLIST (LIST))
	    (foreach a SenTlist
	      (if (= 10 (car a))

		(setq CWLIST (append CWLIST (list (TRANS (cdr a) ZA 0))))
	      )				;IF
	      
	    )				;FOREACH 			

  (IF (> (lcw CWLIST) 0) (command "pedit" sent "r" "" ))
  					(SETQ SENTLIST (ENTGET SENT))

  (if (= lotc nil)
    (progn
      (calclotc cwlist)
      (setq calccen "Y")));calculate lot center if none

 
  

 (SETQ lotno (getstring T (strcat "\n Lot Number [" plotno "]:" )))
  (if (= lotno "") (setq lotno plotno))
       
   (setq area (getstring "\nArea or [C]alculate (mm.dm) (aa.rr.pp.f/p) [Last]:"))
(if (or (= area "")(= area "l")(= area "L")(= area "LAST")(= area "last"))(setq area "Last"))

  (if (= area "Last" )(setq area arealast))
  (setq arealast area)


  ;deal with imperial areas
  
      
	(setq dotpos1 (vl-string-position 46 area 0))
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 area (+ dotpos1 1))))
	(if (/= dotpos2 nil)(progn;idenfited as imperial area, must have second dotpos to work
			      
	(if (= dotpos2 nil)(setq dotpos3 nil)(setq dotpos3 (vl-string-position 46 area (+ dotpos2 1))))
	(setq /pos1 (vl-string-position 47 area 0))
	(if (/= /pos1 nil);with factional part
	  (progn
	    (setq den (substr area ( + /pos1 2) 50))
	    (setq num (substr area ( + dotpos3 2) (- (- /pos1 dotpos3) 1)))
	    (setq fperch (/ (atof num) (atof den)))
	    (setq perch (substr area (+ dotpos2 2) (- (- dotpos3 dotpos2) 1)))
	    (setq perch (+ fperch (atof perch)))
	    (setq rood (substr area (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq perch (+ perch (* (atof rood) 40)))
	    (setq acre (substr area 1  dotpos1 ))
	    (setq perch (+ perch (* (atof acre) 160)))
	    (setq area (rtos (* perch 25.2929538117) 2 9))
	    )
	  )
	(if (and (/= dotpos1 nil)(/= dotpos2 nil)(= /pos1 nil));without fractional part
	  (progn
	    (setq perch (substr area ( + dotpos2 2) 50))
	    (setq perch (atof perch))
	    (setq rood (substr area (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq perch (+ perch (* (atof rood) 40)))
	    (setq acre (substr area 1  dotpos1 ))
	    (setq perch (+ perch (* (atof acre) 160)))
	    (setq area (rtos (* perch 25.2929538117) 2 9))
	    )
	  )
	
	));p&if imperial area

   
   (SETQ area1 (vlax-get-property (vlax-ename->vla-object sent ) 'area ))

  (setvar "dimzin" 0)
  (IF (or ( = area "C")(= area "c"))
    (progn
     (setq area (rtos area1 2 3))
      (setq area1 (atof (rtos area1 2 3)));deal with recurring 9's
      					    (if (> area1 0)(setq textarea (strcat (rtos (* (/ area1 0.1) 0.1) 2 1) "m²")))
					    (if (> area1 100)(setq textarea (strcat (rtos (*  (/ area1 1) 1) 2 0) "m²")))
      					    (if (> area1 10000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 0.001) 0.001) 2 3) "ha")))
					    (if (> area1 100000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 0.01) 0.01) 2 2) "ha")))
					    (if (> area1 1000000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 0.1) 0.1) 2 1) "ha")))
					    (if (> area1 10000000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 1) 1) 2 0) "ha")))
                                            (if (> area1 100000000) (setq textarea (strcat (rtos (* (/ (/ area1 1000000) 0.1) 0.1) 2 1) "km²")))
                                            (if (> area1 1000000000) (setq textarea (strcat (rtos (*  (/ (/ area1 1000000) 1) 1) 2 0) "km²")))
      
					    
      )
    (progn
     (setq areapercent (ABS(* (/  (- area1 (ATOF area)) area1) 100)))
     (if (> areapercent 10) (alert (strcat "\nArea different to calulated by " (rtos areapercent 2 0)"%")))

      				            (if (< (atof area) 0)(setq textarea (strcat (rtos (*  (/ (atof area) 0.1) 0.1) 2 1) "m²")))
					    (if (> (atof area) 100)(setq textarea (strcat (rtos (*  (/ (atof area) 1) 1) 2 0) "m²")))
      					    (if (> (atof area) 10000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 0.001) 0.001) 2 3) "ha")))
					    (if (> (atof area) 100000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 0.01) 0.01) 2 2) "ha")))
					    (if (> (atof area) 1000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 0.1) 0.1) 2 1) "ha")))
					    (if (> (atof area) 10000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 1) 1) 2 0) "ha")))
                                            (if (> (atof area) 100000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 1000000) 0.1) 0.1) 2 1) "km²")))
                                            (if (> (atof area) 1000000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 1000000) 1) 1) 2 0) "km²")))

     )
    )
  (setvar "dimzin" 8)
      (setq lotstate (getstring "\nCreated/Extinguished/Affected/eXisting (C/E/A/X) [C]:"))
  (if (or (= lotstate "c")(= lotstate "C"))(setq lotstate "created"))
  (if (or (= lotstate "e")(= lotstate "E"))(setq lotstate "extinguished"))
  (if (or (= lotstate "a")(= lotstate "A"))(setq lotstate "affected"))
  (if (or (= lotstate "x")(= lotstate "X"))(setq lotstate "existing"))
  (if (= lotstate "")(setq lotstate "created"))

  (if (= (substr lotno 1 2) "PT")(setq pcltype " parcelType=\"Part\""
				     lotno (substr lotno 3 50)
				       ;desc "\" desc=\"PT"
				       textarea (strcat "(" textarea ")")
				       )
    (setq pcltype " parcelType=\"Single\""
	  ;desc ""
	  )
    )

  (if (= calccen "N") (setq lotc (trans lotc 1 0)));convert to world if using UCS

  (if (= (substr lotno 1 2) "CM")(setq oclass "Common Property")(setq oclass "Lot"))
  
    ;<Parcel name="30" class="Lot" state="proposed" parcelType="Single" parcelFormat="Standard" area="951.8">
  (SETQ LTINFO (STRCAT "  <Parcel name=\"" lotno "\" class=\"" oclass "\" state=\"" lotstate "\"" pcltype " area=\""
		       area "\">!" (rtos (cadr lotc) 2 6 ) " " (rtos (car lotc) 2 6)))
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (SETQ NEWSENTLIST (subst (cons 8 "Lot Definitions")(assoc 8 NEWSENTLIST) NEWSENTLIST ))
  (ENTMOD NEWSENTLIST)

  (if (= pcltype " parcelType=\"Part\"")(setq lotnos (strcat "PT" lotno))(setq lotnos  lotno))

(setq lotc (trans lotc 0 1));convert back to UCS if using one

  (SETVAR "CELWEIGHT" 50)
  (if (= lotstate "extinguished")(setvar "clayer" "Drafting AFR")(SETVAR "CLAYER"  "Drafting" ))
  (setq areapos (polar lotc (* 1.5 pi) (* th 2.5)))
  (COMMAND "TEXT" "J" "BC" lotc (* TH 2) "90" lotnos)
  (SETVAR "CELWEIGHT" 35)
  (COMMAND "TEXT" "J" "BC" areapos (* TH 1.4) "90"  textarea )
(SETVAR "CELWEIGHT" -1)
  (if (/= (setq stringpos (vl-string-search "~" lotno )) nil)(setq suffix (substr lotno (+ stringpos 1)))(setq suffix ""))
  (if (/= (atof lotno) 0)(setq plotno (strcat (rtos (+ (atof lotno) 1) 2 0) suffix)))  
  

  (COMMAND "DRAWORDER" SENT "" "BACK")

      (SETVAR "CLAYER" prevlayer)

   (IF (/= areapercent NIL)(PRINC (strcat "\nArea different to calulated by " (rtos areapercent 2 0)"%")))
  );DEFUN



;-------------------------------------------------------------ASSIGN ADJOINING POLYLINE TO XML-----------------------------------

(DEFUN C:XJL (/)

  
  (setq prevlayer (getvar "CLAYER"))
(setq areapercent nil)
  (setq calccen "N")
  (if (= plotno nil) (setq plotno "1"))

 (SETQ SENT (CAR (ENTSEL "\nSelect Polyline:")))
  (setq lotc (getpoint "\nSelect Lot Centre (default Centroid):"))
    
  (SETQ SENTLIST (ENTGET SENT))
    ;go through polyline to get points to check for clockwise direction
  (SETQ ZA (CDR (ASSOC 210 SENTLIST)))
    (SETQ CWLIST (LIST))
	    (foreach a SenTlist
	      (if (= 10 (car a))

		(setq CWLIST (append CWLIST (list (TRANS (cdr a) ZA 0))))
	      )				;IF
	      
	    )				;FOREACH 			

  (IF (> (lcw CWLIST) 0) (command "pedit" sent "r" "" ))
  					(SETQ SENTLIST (ENTGET SENT))

  (if (= lotc nil)
    (progn
      (calclotc cwlist)
      (setq calccen "Y")));calculate lot center if none

  

 (SETQ lotno (getstring T (strcat "\n Lot and PLAN number:")))
  (if (= lotno "") (setq lotno plotno))
       
   
  
  (setvar "dimzin" 8)
    (setq lotstate "existing")
  (if (= lotstate "")(setq lotstate "proposed"))

  (if (= (substr lotno 1 2) "PT")(setq pcltype " parcelType=\"Part\""
				     lotno (substr lotno 3 50)
				       desc "\" desc=\"PT"
				       )
    (setq pcltype " parcelType=\"Single\""
	  desc "")
    )
  (if (= calccen "N")(setq lotc (trans lotc 1 0)));convert to world if using UCS

    ;<Parcel name="30" class="Lot" state="proposed" parcelType="Single" parcelFormat="Standard" area="951.8">
  (SETQ LTINFO (STRCAT "  <Parcel name=\"" lotno desc "\" class=\"Lot\" state=\"" lotstate "\"" pcltype " parcelFormat=\"Standard\">!" (rtos (cadr lotc) 2 6 ) " " (rtos (car lotc) 2 6)))
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
  
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (SETQ NEWSENTLIST (subst (cons 8 "Adjoining Boundary")(assoc 8 NEWSENTLIST) NEWSENTLIST ))
  (ENTMOD NEWSENTLIST)

  (if (= desc "\" desc=\"PT")(setq lotnos (strcat "PT" lotno))(setq lotnos  lotno))


(setq lotc (trans lotc 0 1));convert to world if using UCS
  (SETVAR "CELWEIGHT" -1)
  (SETVAR "CLAYER"  "Drafting" )
  (COMMAND "TEXT" "J" "BC" lotc (* TH 2) "90" lotnos)
  
  
  (COMMAND "DRAWORDER" SENT "" "BACK")

      (SETVAR "CLAYER" prevlayer)

  );DEFUN






;-----------------------------------------------------------------------------Assign Polyline to XML Easement----------------------------------------------------
(defun C:XAE (/)
  (setq calccen "N")

  (setq prevlayer (getvar "CLAYER"))
  (setq pickstyle (getvar "pickstyle"))
(easecount)
  (if (= plotno nil) (setq plotno "1"))
(easecount)
 (SETQ SENT (CAR (ENTSEL "\nSelect Polyline:")))
 (setq lotc (getpoint "\nSelect Lot Centre (default Centroid):"))
    
  (SETQ SENTLIST (ENTGET SENT))
    ;go through polyline to get points to check for clockwise direction
  (SETQ ZA (CDR (ASSOC 210 SENTLIST)))
    (SETQ CWLIST (LIST))
	    (foreach a SenTlist
	      (if (= 10 (car a))

		(setq CWLIST (append CWLIST (list (TRANS (cdr a) ZA 0))))
	      )				;IF
	      
	    )				;FOREACH 			

  (IF (> (lcw CWLIST) 0) (command "pedit" sent "r" "" ))
  					(SETQ SENTLIST (ENTGET SENT))

  (if (= lotc nil)
    (PROGN
      (calclotc cwlist)
      (setq calccen "Y")));calculate lot center if none

  (SETQ easeid (getstring T (STRCAT "\nEasment Identifier (E# or PTE#:")))
  (if (or (= (substr easeid 1 2) "PT")(= (substr easeid 1 2) "pt")(= (substr easeid 1 2) "Pt"))(setq easeid (substr easeid 3)
												     parceltype "Part")
    (setq parceltype "Single")
    )

   (SETQ lotstate (getstring T (STRCAT "\n Created/Existing [C/E](defult is created):")))
  (if (or (= (strcase lotstate) "C")(= (strcase lotstate) "CREATED")(= lotstate ""))(setq lotstate "created"))
  (if (or (= (strcase lotstate) "E")(= (strcase lotstate) "EXISTING"))(setq lotstate "existing"))
  

;(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))

  (SETQ PTLIST (LIST))
				
;CREATE LIST OF POLYLINE POINTS
    (SETQ PTLIST (LIST))
	    (foreach a SENTLIST
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	     
	    )				;FOREACH 			)

(setq ptlist (append ptlist (list(nth 0 ptlist))))
  
  
 (if (= (substr easeid 1 3) "RST")
    (SETQ pclclass "Restriction"
	  pclformat "")
    (setq pclclass "Easement";else normal easement
	  pclformat " parcelFormat=\"Geometry\" ")
    )
   
 (if (= calccen "N") (setq lotc (trans lotc 1 0)));convert to world if using UCS
   
  (SETQ LTINFO (STRCAT "<Parcel name=\"" easeid "\""  " class=\"" pclclass "\" state=\"" lotstate "\" parcelType=\"" parceltype "\"" pclformat  ">!"
		       (rtos (cadr lotc) 2 6 ) " " (rtos (car lotc) 2 6)))


(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (SETQ NEWSENTLIST (subst (cons 8 "Lot Definitions")(assoc 8 NEWSENTLIST) NEWSENTLIST ))
  (ENTMOD NEWSENTLIST)

  (setq THF 1)

  (setq lotc (trans lotc 0 1));convert back to UCS

  (SETVAR "CLAYER"  "Drafting" )

  		      
					
  (COMMAND "TEXT" "J" "MC" lotc (* TH THF) "90" easeid)
       (setq roadname (entget (entlast)))

   (SETVAR "CLAYER" prevlayer)


(setq lotc (trans lotc 1 0));convert to world if using UCS
    
		(setq count 0)
		(setq minoff 100000000000000000000000000000000000000)
(repeat (- (length ptlist)2 )

  (setq op1 (nth count ptlist))
  (setq op2 (nth (+ count 1) ptlist))
  (setq op3 (nth (+ count 2) ptlist))



  ;check line one
;check offset to line
  (SETQ ANG (ANGLE oP1 oP2))
  (SETQ CANG (+ ANG (/ PI 2)))
  (SETQ P4 (POLAR lotc CANG 50))
  (SETQ P6 (POLAR lotc (+ CANG PI) 50))
   
  (IF (SETQ P5 (INTERS oP1 oP2 P6 P4 ))(PROGN
					
 
      (SETQ OFF (DISTANCE lotc P5))

    (if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang cang))
     
    
  ));p & if inters

    ;check inside deflection angle
    (setq ang2 (angle op2 op3))
    (setq cang2 (+ ang (/ pi 2)))
    (setq ltopang (angle op2 lotc))
    (setq defl (- ang2 ang))
(if (/= defl 0);check for straight line
  (progn
    (if (< defl pi)(progn
		     (setq ray1 (- ang (/ pi 2)))
		     ;(if (< ray1 0) (setq ray1 (+ ray1 (* 2 pi))))
		     (setq ray2 (- ang2 (/ pi 2)))
		     ;(if (< ray2 0) (setq ray2 (+ ray2 (* 2pi))))
		     ));p and if less than pi
    (if (> defl pi)(progn
		     (setq ray1 (+ ang (/ pi 2)))
		     ;(if (> ray1 (* 2 pi)(setq ray1 (- ray1 (* 2 pi)))))
		     (setq ray2 (+ ang (/ pi 2)))
		     ;(if (> ray2 (* 2 pi)(setq ray2 (- ray2 (* 2 pi)))))
		     ));p and if greater than pi

    (if (or (and (> ltopang ray1) (< ltopang ray2))(and (> ltopang ray2)(< ltopang ray1)));check ot see if inside deflection squares **** needs testing later
      (progn
	(setq off (distance lotc op2))
(if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang ltopang)
      );if
	));p and if in defl sqr
   ));p and if not straight line

  (setq count (+ count 1))
    );repeat
      
				;using this info change the road name text rotation to the angle of the line
		(setq rrot (+ nearang (/ pi 2)))
		(if (> rrot (* 2 pi))(setq rrot (- rrot (* 2 pi))))
		(IF (AND (> rrot  (* 0.5 pi)) (< rrot (* 1.5 pi)))(setq rrot (+ rrot pi))) ;if text is upsidedown reverse rotation
			  (setq p1 (polar lotc rrot 10))
  (setq llotc (trans lotc 0 1))
  (setq lp1 (trans p1 0 1))
  (setq oang (angle llotc lp1))
  (IF (AND (> oang  (* 0.5 pi)) (< oang (* 1.5 pi)))(setq rrot (+ rrot pi))) ;if text is upsidedown reverse rotation		  
		
    
  (SETQ	roadname (subst (cons 50  rrot);SUB IN NEW POINT 2 HEIGHT
		     (assoc 50 roadname)
		     roadname     ) )
  (ENTMOD roadname)
(COMMAND "DRAWORDER" SENT "" "BACK")


  (setvar "pickstyle" pickstyle)
  )
  


;------------------------------------------------------Assign polyline to XML Road----------------------------------------------

(defun C:XAR (/)

 (setq prevlayer (getvar "CLAYER"))
  (setq calccen "N")

  (if (= plotno nil) (setq plotno "1"))

 (SETQ SENT (CAR (ENTSEL "\nSelect Polyline:")))
  (setq lotc (getpoint "\nSelect Lot Centre (default Centroid):"))
    
  (SETQ SENTLIST (ENTGET SENT))
    ;go through polyline to get points to check for clockwise direction
  (SETQ ZA (CDR (ASSOC 210 SENTLIST)))
    (SETQ CWLIST (LIST))
	    (foreach a SenTlist
	      (if (= 10 (car a))

		(setq CWLIST (append CWLIST (list (TRANS (cdr a) ZA 0))))
	      )				;IF
	      
	    )				;FOREACH 			

  (IF (> (lcw CWLIST) 0) (command "pedit" sent "r" "" ))
  					(SETQ SENTLIST (ENTGET SENT))

  (if (= lotc nil)
    (progn
      (calclotc cwlist)
      (setq calccen "Y")));calculate lot center if none

  (SETQ PTLIST (LIST))
				
;CREATE LIST OF POLYLINE POINTS
    (SETQ PTLIST (LIST))
	    (foreach a SENTLIST
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	     
	    )				;FOREACH 			)


  (setq ptlist (append ptlist (list (nth 0 ptlist))))



(SETQ lotno (getstring T"\n Road/Reserve Identifier (eg R#/RES#):"))
  (SETQ desc (getstring T"\n Road Name:"))
 (SETQ pclowner (getstring T (STRCAT "\n Council/Body/Person [" prevowner "]:")))
  (if (= pclowner "")(setq pclowner prevowner))
  (setq prevowner pclowner)

  (setq area (getstring "\nArea or [C]alculate (mm.dm) (aa.rr.pp.f/p) [Last]:"))
(if (or (= area "")(= area "l")(= area "L")(= area "LAST")(= area "last"))(setq area "Last"))

  (if (= area "Last" )(setq area arealast))
  (setq arealast area)


  ;deal with imperial areas
  
      
	(setq dotpos1 (vl-string-position 46 area 0))
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 area (+ dotpos1 1))))
	(if (/= dotpos2 nil)(progn;idenfited as imperial area, must have second dotpos to work
			      
	(if (= dotpos2 nil)(setq dotpos3 nil)(setq dotpos3 (vl-string-position 46 area (+ dotpos2 1))))
	(setq /pos1 (vl-string-position 47 area 0))
	(if (/= /pos1 nil);with factional part
	  (progn
	    (setq den (substr area ( + /pos1 2) 50))
	    (setq num (substr area ( + dotpos3 2) (- (- /pos1 dotpos3) 1)))
	    (setq fperch (/ (atof num) (atof den)))
	    (setq perch (substr area (+ dotpos2 2) (- (- dotpos3 dotpos2) 1)))
	    (setq perch (+ fperch (atof perch)))
	    (setq rood (substr area (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq perch (+ perch (* (atof rood) 40)))
	    (setq acre (substr area 1  dotpos1 ))
	    (setq perch (+ perch (* (atof acre) 160)))
	    (setq area (rtos (* perch 25.2929538117) 2 9))
	    )
	  )
	(if (and (/= dotpos1 nil)(/= dotpos2 nil)(= /pos1 nil));without fractional part
	  (progn
	    (setq perch (substr area ( + dotpos2 2) 50))
	    (setq perch (atof perch))
	    (setq rood (substr area (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq perch (+ perch (* (atof rood) 40)))
	    (setq acre (substr area 1  dotpos1 ))
	    (setq perch (+ perch (* (atof acre) 160)))
	    (setq area (rtos (* perch 25.2929538117) 2 9))
	    )
	  )
	
	));p&if imperial area

   
   (SETQ area1 (vlax-get-property (vlax-ename->vla-object sent ) 'area ))

  (setvar "dimzin" 0)
  (IF (or ( = area "C")(= area "c"))
    (progn
     (setq area (rtos area1 2 3))
      (setq area1 (atof (rtos area1 2 3)));deal with recurring 9's
      					    (if (> area1 0)(setq textarea (strcat (rtos (* (/ area1 0.1) 0.1) 2 1) "m²")))
					    (if (> area1 100)(setq textarea (strcat (rtos (*  (/ area1 1) 1) 2 0) "m²")))
      					    (if (> area1 10000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 0.001) 0.001) 2 3) "ha")))
					    (if (> area1 100000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 0.01) 0.01) 2 2) "ha")))
					    (if (> area1 1000000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 0.1) 0.1) 2 1) "ha")))
					    (if (> area1 10000000) (setq textarea (strcat (rtos (*  (/ (/ area1 10000) 1) 1) 2 0) "ha")))
                                            (if (> area1 100000000) (setq textarea (strcat (rtos (* (/ (/ area1 1000000) 0.1) 0.1) 2 1) "km²")))
                                            (if (> area1 1000000000) (setq textarea (strcat (rtos (*  (/ (/ area1 1000000) 1) 1) 2 0) "km²")))
      
					    
      )
    (progn
     (setq areapercent (ABS(* (/  (- area1 (ATOF area)) area1) 100)))
     (if (> areapercent 10) (alert (strcat "\nArea different to calulated by " (rtos areapercent 2 0)"%")))

      				            (if (< (atof area) 0)(setq textarea (strcat (rtos (*  (/ (atof area) 0.1) 0.1) 2 1) "m²")))
					    (if (> (atof area) 100)(setq textarea (strcat (rtos (*  (/ (atof area) 1) 1) 2 0) "m²")))
      					    (if (> (atof area) 10000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 0.001) 0.001) 2 3) "ha")))
					    (if (> (atof area) 100000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 0.01) 0.01) 2 2) "ha")))
					    (if (> (atof area) 1000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 0.1) 0.1) 2 1) "ha")))
					    (if (> (atof area) 10000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 10000) 1) 1) 2 0) "ha")))
                                            (if (> (atof area) 100000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 1000000) 0.1) 0.1) 2 1) "km²")))
                                            (if (> (atof area) 1000000000) (setq textarea (strcat (rtos (*  (/ (/ (atof area) 1000000) 1) 1) 2 0) "km²")))

     )
    )
  
   
  (if (= calccen "N")(setq lotc (trans lotc 1 0)));convert to world if using UCS
(if (= (substr lotno 1 3) "RES")(setq pclclass "Reserve")(setq pclclass "Road"))
  (if (/= desc "")(setq odesc (strcat " desc=\"" desc "\"")))
  (SETQ LTINFO (STRCAT "<Parcel name=\"" lotno "\"" odesc " area=\"" area "\" class=\"" pclclass "\" state=\"created\" parcelType=\"Single\" parcelFormat=\"Standard\" owner=\"" pclowner "\">!"
		       (rtos (cadr lotc) 2 6 ) " " (rtos (car lotc) 2 6)))
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
   (SETQ NEWSENTLIST (subst (cons 8 "Lot Definitions")(assoc 8 NEWSENTLIST) NEWSENTLIST ))
  (ENTMOD NEWSENTLIST)

  (setq THF 2)
(setq lotc (trans lotc 0 1));convert back to UCS
  
  (setq rpos (polar lotc (* 1.5 pi) (* th 2.5)))
  (setq areapos (polar lotc (* 1.5 pi) (* th 5)))
  (if (/= desc "ROAD")(PROGN (SETVAR "CELWEIGHT" 50)
			(SETQ RTH (* TH 2)))
    (SETQ RTH (* TH 1.4)))
  (SETVAR "CLAYER"  "Drafting" )
  (IF (/= "" desc )
    (progn
  (COMMAND "TEXT" "J" "BC" lotc RTH "90" desc)
  (setq roadname (entget (entlast)))
  ))
  (SETVAR "CELWEIGHT" 35)
  (COMMAND "TEXT" "J" "BC" areapos (* TH 1.4) "90"  textarea )
  (COMMAND "TEXT" "J" "BC" rpos (* TH 1.4) "90"  lotno )
(SETVAR "CELWEIGHT" -1)
 


(setq lotc (trans lotc 1 0));convert to world if using UCS

  (if (/= desc "")
    (progn
		(setq count 0)
		(setq minoff 100000000000000000000000000000000000000)
(repeat (- (length ptlist)2 )

  (setq op1 (nth count ptlist))
  (setq op2 (nth (+ count 1) ptlist))
  (setq op3 (nth (+ count 2) ptlist))



  ;check line one
;check offset to line
  (SETQ ANG (ANGLE oP1 oP2))
  (SETQ CANG (+ ANG (/ PI 2)))
  (SETQ P4 (POLAR lotc CANG 1000))
  (SETQ P6 (POLAR lotc (+ CANG PI) 2000))
  (IF (SETQ P5 (INTERS oP1 oP2 P6 P4 ))(PROGN

					 (SETQ OFF (DISTANCE lotc P5))

    (if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang cang)
      );if
    
  ));p & if inters

    ;check inside deflection angle
    (setq ang2 (angle op2 op3))
    (setq cang2 (+ ang (/ pi 2)))
    (setq ltopang (angle op2 lotc))
    (setq defl (- ang2 ang))
(if (/= defl 0);check for straight line
  (progn
    (if (< defl pi)(progn
		     (setq ray1 (- ang (/ pi 2)))
		     ;(if (< ray1 0) (setq ray1 (+ ray1 (* 2 pi))))
		     (setq ray2 (- ang2 (/ pi 2)))
		     ;(if (< ray2 0) (setq ray2 (+ ray2 (* 2pi))))
		     ));p and if less than pi
    (if (> defl pi)(progn
		     (setq ray1 (+ ang (/ pi 2)))
		     ;(if (> ray1 (* 2 pi)(setq ray1 (- ray1 (* 2 pi)))))
		     (setq ray2 (+ ang (/ pi 2)))
		     ;(if (> ray2 (* 2 pi)(setq ray2 (- ray2 (* 2 pi)))))
		     ));p and if greater than pi

    (if (or (and (> ltopang ray1) (< ltopang ray2))(and (> ltopang ray2)(< ltopang ray1)));check ot see if inside deflection squares **** needs testing later
      (progn
	(setq off (distance lotc op2))
(if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang ltopang)
      );if
	));p and if in defl sqr
   ));p and if not straight line

  (setq count (+ count 1))
    );repeat
      
				;using this info change the road name text rotation to the angle of the line
		(setq rrot (+ nearang (/ pi 2)))
		(if (> rrot (* 2 pi))(setq rrot (- rrot (* 2 pi))))
		(IF (AND (> rrot  (* 0.5 pi)) (< rrot (* 1.5 pi)))(setq rrot (+ rrot pi))) ;if text is upsidedown reverse rotation

  (setq p1 (polar lotc rrot 10))
  (setq llotc (trans lotc 0 1))
  (setq lp1 (trans p1 0 1))
  (setq oang (angle llotc lp1))
  (IF (AND (> oang  (* 0.5 pi)) (< oang (* 1.5 pi)))(setq rrot (+ rrot pi))) ;if text is upsidedown reverse rotation		  
		
    
  (SETQ	roadname (subst (cons 50  rrot);SUB IN NEW POINT 2 HEIGHT
		     (assoc 50 roadname)
		     roadname     ) )
  (ENTMOD roadname)
  
   (SETQ roadname (subst (cons 8  "Drafting AFR")(assoc 8 roadname) roadname))
	       (SETQ roadname (subst (cons 50  rrot )(assoc 50 roadname) roadname))
		(ENTMAKE roadname)
      ))

(COMMAND "DRAWORDER" SENT "" "BACK")



  
   ;get admin sheet if exist and store rlp point
  (IF (= RDLP nil)(progn
(IF  (/= (setq adminsheet (ssget "_X" '((0 . "INSERT") (2 . "ADMINSHEET")))) nil)(progn
		(SETQ p1 (CDR(ASSOC 10 (ENTGET (SSNAME ADMINSHEET 0)))))
		(SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME ADMINSHEET 0)))))
		(setq RDLP (list (+ (CAR P1) 28) (+ (CADR P1) 255)));setup postion for easement legend
))
(IF (= RDLP NIL)(SETQ RDLP (GETPOINT "Road Table Position:")))
))
  
(COMMAND "TEXT" "J" "BL" RDLP 2.5 "90"  LOTNO )
(COMMAND "TEXT" "J" "BL" (LIST (+ (CAR RDLP) 37)(CADR RDLP)) 2.5 "90" PCLOWNER )
  (SETQ RDLP (LIST (CAR RDLP)(- (CADR RDLP) 4)))


  (SETVAR "CLAYER" prevlayer) 
(COMMAND "DRAWORDER" ENTSS "" "BACK")
(setvar "pickstyle" pickstyle)
  


  )



;------------------------------------------------------Assign Adjoining polyline to XML Road----------------------------------------------

(defun C:XJR (/)

 (setq prevlayer (getvar "CLAYER"))
  (setq calccen "N")

  (if (= plotno nil) (setq plotno "1"))

 (SETQ SENT (CAR (ENTSEL "\nSelect Polyline:")))
  (setq lotc (getpoint "\nSelect Lot Centre (default Centroid):"))
    
  (SETQ SENTLIST (ENTGET SENT))
    ;go through polyline to get points to check for clockwise direction
  (SETQ ZA (CDR (ASSOC 210 SENTLIST)))
    (SETQ CWLIST (LIST))
	    (foreach a SenTlist
	      (if (= 10 (car a))

		(setq CWLIST (append CWLIST (list (TRANS (cdr a) ZA 0))))
	      )				;IF
	      
	    )				;FOREACH 			

  (IF (> (lcw CWLIST) 0) (command "pedit" sent "r" "" ))
  					(SETQ SENTLIST (ENTGET SENT))

  (if (= lotc nil)
    (progn
      (calclotc cwlist)
      (setq calccen "Y")
      ));calculate lot center if none


  (SETQ PTLIST (LIST))
				
;CREATE LIST OF POLYLINE POINTS
    (SETQ PTLIST (LIST))
	    (foreach a SENTLIST
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	     
	    )				;FOREACH 			)


  (setq ptlist (append ptlist (list (nth 0 ptlist))))
  
 (SETQ lotno (getstring T"\n Road Name:"))
  
    
  (setq lotstate "adjoining")
    (setq lotc (trans lotc 1 0));convert to world if using UCS
  (SETQ LTINFO (STRCAT "desc=\"" lotno "\" class=\"Road\" state=\"existing\" parcelType=\"Single\" parcelFormat=\"Standard\">!"
		       (rtos (cadr lotc) 2 6 ) " " (rtos (car lotc) 2 6)))
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (SETQ NEWSENTLIST (subst (cons 8 "Adjoining Boundary")(assoc 8 NEWSENTLIST) NEWSENTLIST ))
  (ENTMOD NEWSENTLIST)

  (setq THF 2)
(setq lotc (trans lotc 0 1));convert back to UCS
  (SETVAR "CLAYER"  "Drafting" )
	(SETVAR "CELWEIGHT" 50)				
  (COMMAND "TEXT" "J" "MC" lotc (* TH THF) "90" lotno)
       (setq roadname (entget (entlast)))
(SETVAR "CELWEIGHT" -1)
   (SETVAR "CLAYER" prevlayer) 


(setq lotc (trans lotc 1 0));convert to world if using UCS
    
		(setq count 0)
		(setq minoff 100000000000000000000000000000000000000)
(repeat (- (length ptlist)2 )

  (setq op1 (nth count ptlist))
  (setq op2 (nth (+ count 1) ptlist))
  (setq op3 (nth (+ count 2) ptlist))



  ;check line one
;check offset to line
  (SETQ ANG (ANGLE oP1 oP2))
  (SETQ CANG (+ ANG (/ PI 2)))
  (SETQ P4 (POLAR lotc CANG 1000))
  (SETQ P6 (POLAR lotc (+ CANG PI) 2000))
  (IF (SETQ P5 (INTERS oP1 oP2 P6 P4 ))(PROGN

					 (SETQ OFF (DISTANCE lotc P5))

    (if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang cang)
      );if
    
  ));p & if inters

    ;check inside deflection angle
    (setq ang2 (angle op2 op3))
    (setq cang2 (+ ang (/ pi 2)))
    (setq ltopang (angle op2 lotc))
    (setq defl (- ang2 ang))
(if (/= defl 0);check for straight line
  (progn
    (if (< defl pi)(progn
		     (setq ray1 (- ang (/ pi 2)))
		     ;(if (< ray1 0) (setq ray1 (+ ray1 (* 2 pi))))
		     (setq ray2 (- ang2 (/ pi 2)))
		     ;(if (< ray2 0) (setq ray2 (+ ray2 (* 2pi))))
		     ));p and if less than pi
    (if (> defl pi)(progn
		     (setq ray1 (+ ang (/ pi 2)))
		     ;(if (> ray1 (* 2 pi)(setq ray1 (- ray1 (* 2 pi)))))
		     (setq ray2 (+ ang (/ pi 2)))
		     ;(if (> ray2 (* 2 pi)(setq ray2 (- ray2 (* 2 pi)))))
		     ));p and if greater than pi

    (if (or (and (> ltopang ray1) (< ltopang ray2))(and (> ltopang ray2)(< ltopang ray1)));check ot see if inside deflection squares **** needs testing later
      (progn
	(setq off (distance lotc op2))
(if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang ltopang)
      );if
	));p and if in defl sqr
   ));p and if not straight line

  (setq count (+ count 1))
    );repeat
      
				;using this info change the road name text rotation to the angle of the line
		(setq rrot (+ nearang (/ pi 2)))
		(if (> rrot (* 2 pi))(setq rrot (- rrot (* 2 pi))))
		(IF (AND (> rrot  (* 0.5 pi)) (< rrot (* 1.5 pi)))(setq rrot (+ rrot pi))) ;if text is upsidedown reverse rotation
			  (setq p1 (polar lotc rrot 10))
  (setq llotc (trans lotc 0 1))
  (setq lp1 (trans p1 0 1))
  (setq oang (angle llotc lp1))
  (IF (AND (> oang  (* 0.5 pi)) (< oang (* 1.5 pi)))(setq rrot (+ rrot pi))) ;if text is upsidedown reverse rotation		  
		
    
  (SETQ	roadname (subst (cons 50  rrot);SUB IN NEW POINT 2 HEIGHT
		     (assoc 50 roadname)
		     roadname     ) )
  (ENTMOD roadname)
  	       (SETQ roadname (subst (cons 8  "Drafting AFR")(assoc 8 roadname) roadname))
	       (SETQ roadname (subst (cons 50  rrot )(assoc 50 roadname) roadname))
		(ENTMAKE roadname)
  
(COMMAND "DRAWORDER" SENT "" "BACK")
  )


;-------------------------------------------------------------ASSIGN POLYLINE AS IRREGULAR BOUNDARY-----------------------------------

(DEFUN C:XAI (/)

  
  (setq prevlayer (getvar "CLAYER"))

 (SETQ select  (ENTSEL "\nSelect Polyline:"))
 ; (setq dsp (getpoint "\nSelect downstream point (if non tidal):"))
  ;(setq dsp (trans dsp 1 0))
  ;(if (= dsp nil)(setq dfa "DFAT")(setq dfa "DFANT"))
  (setq sent (car select))
  (setq irbd (gETSTRING T "\nIrregular Boundary Description:"))
  


(setq pltype (cdr (assoc 0 (ENTGET SENT))))
(SETQ SENTLIST (ENTGET SENT))
  
  (if (= pltype "LWPOLYLINE")(PROGN
  
  
  (SETQ PTLIST (LIST))
	    (foreach a sentlist
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	      
	    )				;FOREACH 			
)
    );IF LWPOLYLINE

  (if (= pltype "POLYLINE")(PROGN

  (setq enlist (entget SenT))
	    (setq ptList (list));EMPTY LIST
	    (setq en2 (entnext SenT))
	    (setq enlist2 (entget en2))
               
	     (while
	      (not
		(equal (cdr (assoc 0 (entget (entnext en2)))) "SEQEND")
	      )
	      	(if (= (cdr(assoc 70 enlist2)) 16)(progn
	       	 (setq ptList (append ptList (list (cdr (assoc 10 enlist2)))))
		))
		 	       
	       (setq en2 (entnext en2))
	       (setq enlist2 (entget en2))
	       );W
   (setq ptList
			(append ptList (list (cdr (assoc 10 enlist2))))
		 
	       )
  ));IF 2D POLYLINE
			     
  
		 (setq mp1 (nth (- (/ (length ptlist) 2)1) ptlist))
		 (setq mp2 (nth  (/ (length ptlist) 2) ptlist))
		 (setq mp (list (/ (+ (car mp1)(car mP2)) 2) (/ (+ (cadr mp1)(cadr mp2)) 2)))
		 (setq mprot (angle mp1 mp2))
		 (setq mprot90 (+ mprot (* 0.5 pi)))
  		 (SETQ 1POS (POLAR mp mprot90 (* TH 2)))
                 (SETQ fap (POLAR mp mprot90 (* TH 5)))
                 (IF (AND (> mprot  (* 0.5 pi)) (< mprot (* 1.5 pi)))(setq mprot (+ mprot pi))) ;if text is upsidedown reverse rotation
                 (setvar "clayer" "Drafting")
		 (COMMAND "TEXT" "J" "MC" 1pos TH (ANGTOS mprot 1 4) irbd)
;figure out direction of flow arrow

 ; (if (/= dsp nil)(setq dspc (strcat (rtos (car dsp) 2 6)","(rtos (cadr dsp) 2 6))))
 ; (setq plp1 (strcat (rtos (car (nth 0 ptlist)) 2 6)","(rtos (cadr (nth 0 ptlist)) 2 6)))
 ; (setq plp2 (strcat (rtos (car (nth (- (length ptlist) 1) ptlist)) 2 6)","(rtos (cadr (nth (- (length ptlist) 1) ptlist)) 2 6)))
	
  
;(if (= dspc plp1) (setq far (angle mp2 mp1)
			;dfapoints (strcat plp2 "!" plp1)))
;(if (= dspc plp2) (setq far (angle mp1 mp2)
;			dfapoints (strcat plp1 "!" plp2)))

 ; (if (= dsp nil)(setq far (angle mp1 mp2)
;		       dfapoints (strcat plp1 "!" plp2)))
 ; (if (and (/= dspc plp1)(/= dspc plp2))(setq dfa "DFAT"))
                 
  ;               (command "insert" dfa fap th (angtos far))
;(SETQ DFAENT (ENTGET (entlast)))
;  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 dfapoints)))))
;   (setq NEWSENTLIST (APPEND DFAENT XDATA))
;  (ENTMOD NEWSENTLIST)
  
		 (setvar "clayer" prevlayer)
  

  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 irbd)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (SETQ NEWSENTLIST (subst (cons 8 "Irregular Boundary")(assoc 8 NEWSENTLIST) NEWSENTLIST ))
  (ENTMOD NEWSENTLIST)



  ;@@@@@add tool to find middle point and label irregular line or use sp and ep
  
  );DEFUN



;-------------------------------------------------------------ASSIGN POLYLINE AS OCCUPATION-----------------------------------

(DEFUN C:XAO (/)

  
  (setq prevlayer (getvar "CLAYER"))

 (SETQ select  (ENTSEL "\nSelect Object:"))
  (setq sent (car select))
  (setq irbd (gETSTRING T "\nOccupation Boundary Description:"))


(setq pltype (cdr (assoc 0 (ENTGET SENT))))
  
(SETQ SENTLIST (ENTGET SENT))
    
  (if (= pltype "LWPOLYLINE")(PROGN

			       ;if occupations layer append description in front of chainage list
			       (if (= (cdr (assoc 8 (ENTGET SENT))) "Occupations")
				 (progn
				     (SETQ XDATAI (ENTGET sent '("LANDXML")))
	   (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     
	      (princ (strcat "\nChainage object has no chainages"))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
				   (if (/= (substr xdatai 1 2) "00");if description already exists
				     (progn
				     (setq ,pos1 (vl-string-position 44 xdatai 0))
				     (setq xdatai (substr xdatai (+ ,pos1 2) 2000000))
				     ))
				      
				   (setq irbd (strcat irbd "," xdatai))
				   ));if occupaton
				   
  
  (SETQ PTLIST (LIST))
	    (foreach a sentlist
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	      
	    )				;FOREACH 			
)
    );IF LWPOLYLINE

  (if (= pltype "POLYLINE")(PROGN


			     (setq enlist (entget SenT))
	    (setq ptList (list));EMPTY LIST
	    (setq en2 (entnext SenT))
	    (setq enlist2 (entget en2))
               
	     (while
	      (not
		(equal (cdr (assoc 0 (entget (entnext en2)))) "SEQEND")
	      )
	      	(if (= (cdr(assoc 70 enlist2)) 16)(progn
	       	 (setq ptList (append ptList (list (cdr (assoc 10 enlist2)))))
		))
		 	       
	       (setq en2 (entnext en2))
	       (setq enlist2 (entget en2))
	       );W
   (setq ptList
			(append ptList (list (cdr (assoc 10 enlist2))))
		 
	       )
  ));IF 2D POLYLINE

   (if (= pltype "LINE")(PROGN

  (setq PTLIST (LIST (cdr (assoc 10 (ENTGET SENT)))))
  (SETQ PTLIST (append ptList (list (cdr (assoc 11 (ENTGET SENT))))))
		 
	       
  ));IF LINE

  (if (= pltype "ARC")(PROGN
			 
(SETQ CP (CDR (assoc 10 (ENTGET SENT))))
  (SETQ RADIUS (CDR (assoc 40 (ENTGET SENT))))
  (SETQ ANG1 (CDR (assoc 50 (ENTGET SENT))))
  (SETQ ANG2 (CDR (assoc 51 (ENTGET SENT))))
  (SETQ P1 (POLAR CP ANG1 RADIUS))
  (SETQ P2 (POLAR CP ANG2 RADIUS))
(SETQ PTLIST (LIST P1))
(SETQ PTLIST (APPEND PTLIST (LIST P2)))
));IF ARC
  
	(if (/= (cdr (assoc 8 (ENTGET SENT))) "Occupations")
	  (progn
  
		 (setq mp1 (TRANS (nth (- (/ (length ptlist) 2)1) ptlist) 0 1))
		 (setq mp2 (TRANS (nth  (/ (length ptlist) 2) ptlist) 0 1))
		 (setq mp (list (/ (+ (car mp1)(car mP2)) 2) (/ (+ (cadr mp1)(cadr mp2)) 2)))
		 (setq mprot (angle mp1 mp2))
		 (setq mprot90 (+ mprot (* 0.5 pi)))
  		 (SETQ 1POS (POLAR mp mprot90 (* TH 2.5)))
                 (IF (AND (> mprot  (* 0.5 pi)) (< mprot (* 1.5 pi)))(setq mprot (+ mprot pi))) ;if text is upsidedown reverse rotation
                 (setvar "clayer" "Drafting AFR")
  		 (COMMAND "TEXT" "J" "MC" 1pos TH (ANGTOS mprot 1 4) irbd)
		 (setvar "clayer" prevlayer)
	    ))
  

  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 irbd)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
   (ENTMOD NEWSENTLIST)

    
  );DEFUN




;-------------------------------------------------------------------Add Address to Parcel-------------


(defun C:XAD (/)
  

(PRINC "\n Select lot to assign address to:")
  
  
  (setq bdyline (ssget  '((0 . "LWPOLYLINE,POINT") (8 . "Lot Definitions"))))
  (setq smalladdress (getstring "\nSmall address:" t))
  (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nObject has not xdata " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))



    (if (/= (setq stringpos (vl-string-search "<smalladdress" xdatai )) nil)(progn
(setq wwpos (vl-string-position 62 xdatai (+ stringpos 15)))(setq stringpos1  stringpos
								  stringpos2 (+ wwpos 2) ));p
      
(progn

 ;else
  (setq stringpossearch 1)
  (while (/= nil (setq stringpossearch (vl-string-search ">" xdatai (+ stringpossearch 1))))
    (setq stringpos stringpossearch)
    )
    
  (setq stringpos1 (+ stringpos 1)
	stringpos2 (+ stringpos 2) 
	)
  )
      )
  
    
(setq xdatafront (substr xdatai 1  stringpos1 ))
    (setq xdataback (substr xdatai stringpos2 ))
    (setq xdatai (strcat xdatafront "<smalladdress=\"" smalladdress "\">" xdataback))

    (SETQ SENTLIST (ENTGET EN))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 xdatai)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

    (setq count (+ count 1))
    )
  )



;-------------------------------------------------------------------Add Title to Parcel-------------


(defun C:XTI (/)
  

(PRINC "\n Select lots to assign title to:")
  
  
  (setq bdyline (ssget  '((0 . "LWPOLYLINE") (8 . "Lot Definitions"))))
  			       
   (setq titletype (getstring (strcat "\nTitle Type(* for list)(default is Freehold):")))
   (if (= titletype "*")(progn
			    (setq workingselnum cselnum)
		      (setq names titlelist)
		      (dbox)
		      (setq titletype returntype)
			    (setq cselnum workingselnum)
		      )
    )
  (if (= titletype "") (setq titletype "Freehold"))
  

  (if (= (member titletype titlelist) nil) (progn
					     (Alert "\nTitle not fount, please choose from list" )
					     (setq workingselnum cselnum)
					     (setq names titlelist)
		      (dbox)
		      (setq titletype returntype)
					     (setq cselnum workingselnum)
					     )
    )

   (setq titlename (getstring (strcat "\nTitle Name(VOL/FOLIO):")))

  
  (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nObject has not xdata " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))



   
 ;else
  (setq stringpossearch 1)
  (while (/= nil (setq stringpossearch (vl-string-search ">" xdatai (+ stringpossearch 1))))
    (setq stringpos stringpossearch)
    )
    
  (setq stringpos1 (+ stringpos 1)
	stringpos2 (+ stringpos 2) 
	)
  
      
  
    
(setq xdatafront (substr xdatai 1  stringpos1 ))
    (setq xdataback (substr xdatai stringpos2 ))
    (setq xdatai (strcat xdatafront "<Title name=\"" titlename "\" titleType\=\"" titletype "\" />" xdataback))

    (SETQ SENTLIST (ENTGET EN))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 xdatai)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

    (setq count (+ count 1))
    )
  )



;-------------------------------------------------------------------Add Distance standard deviation to Line-------------


(defun C:XSDD (/)
  

(PRINC "\n Select Lines to Std Dev to:")
  
  
  (setq bdyline (ssget  '((0 . "LINE") )))
  (setq csf (getstring "\nStd Dev (Const+PPM eg 10+60 ):"))
  (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nObject has not xdata " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))



    (if (/= (setq stringpos (vl-string-search "distanceAccClass" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 18)))
(setq stringpos1 (- stringpos 1)
       stringpos2 (+ wwpos 2) )
);p
      
(progn

 ;else
    (setq stringpos (vl-string-search ">" xdatai ))
  (setq stringpos/ (vl-string-search "/" xdatai))
  (if (= (- stringpos stringpos/) 1)(setq stringpos1 (- stringpos 1)
					  stringpos2 (- stringpos 1))
    (set stringpos1  stringpos 
	 stringpos2  stringpos ))
  
	
  )
      )
  
    
(setq xdatafront (substr xdatai 1  stringpos1 ))
    (setq xdataback (substr xdatai stringpos2 ))
    (setq xdatai (strcat xdatafront " distanceAccClass=\"" csf "\"" xdataback))

    (SETQ SENTLIST (ENTGET EN))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 xdatai)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
    (COMMAND "CHANGE" en "" "P" "LT" "PARISH BOUNDARY" "")

    (setq count (+ count 1))
    )
  )




;-------------------------------------------------------------------Add Bearing standard deviation to Line-------------


(defun C:XSDB (/)
  

(PRINC "\n Select Lines to Std Dev to:")
  
  
  (setq bdyline (ssget  '((0 . "LINE") )))
  (setq csf (getstring "\nStd Dev in Seconds:"))
  (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nObject has not xdata " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))



    (if (/= (setq stringpos (vl-string-search "angleAccClass" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 15)))
(setq stringpos1 (- stringpos 1)
       stringpos2 (+ wwpos 2) )
);p
      
(progn

 ;else
    (setq stringpos (vl-string-search ">" xdatai ))
  (setq stringpos/ (vl-string-search "/" xdatai))
  (if (= (- stringpos stringpos/) 1)(setq stringpos1 (- stringpos 1)
					  stringpos2 (- stringpos 1))
    (set stringpos1  stringpos 
	 stringpos2  stringpos ))
  
	
  )
      )
  
    
(setq xdatafront (substr xdatai 1  stringpos1 ))
    (setq xdataback (substr xdatai stringpos2 ))
    (setq xdatai (strcat xdatafront " angleAccClass=\"" csf "\"" xdataback))

    (SETQ SENTLIST (ENTGET EN))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 xdatai)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
    (COMMAND "CHANGE" en "" "P" "LT" "PARISH BOUNDARY" "")

    (setq count (+ count 1))
    )
  )










;-------------------------------------------------------------------Assign Multitext Store Xdata Easement width, plan note, restrition description-------------


(defun C:XAM (/)
  

(PRINC "\n Select object to assign xdata to:")
  (setq bdyline (ssget  '((0 . "MTEXT"))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline 0)))))
  (setq mttype (getstring "Plan Note(P), Restriction Desc(R):"))
  (if (= (strcase mttype) "R")(SETQ xdatai (getstring "Restriction Name:")))
  (if (= (strcase mttype) "P")(progn
	(setq pntype (getstring (strcat "\nPlan Note Type(* for list):")))
   (if (= pntype "*")(progn
			    (setq workingselnum cselnum)
		      (setq names notelist)
		      (dbox)
		      (setq pntype returntype)
			    (setq cselnum workingselnum)
		      )
    )
	(setq pnlot (getstring "Lot notation applies to (default is none):"))
	(if (/= pnlot "")(setq xdatai (strcat "<Annotation type=\"" pntype "\" pclRef=\"" pnlot "\""))
	  (setq xdatai (strcat "<Annotation type=\"" pntype "\""))
	  )
	));p&if P

  


    (SETQ SENTLIST (ENTGET EN))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 xdatai)))))
  (SETQ SENTLIST (subst (cons 8 "Admin Sheet")(assoc 8 SENTLIST) SENTLIST ))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

  
  )






;-------------------------------------------------------------SHORT LINE TABLE-----------------------------------

(DEFUN C:XSL (/)

  (setq prevlayer (getvar "CLAYER"))
(SETQ COMMENT "")
 (SETQ LINES (SSGET  '((0 . "LINE"))))

  (IF (/= SLCOUNT NIL)  (SETQ PSLCOUNT SLCOUNT)(SETQ SLCOUNT ""
						     PSLCOUNT ""))
  (SETQ SLCOUNT (GETSTRING (STRCAT "\nShort Line Starting Number (" SLCOUNT "):" )))
  (IF (= SLCOUNT "" )(SETQ SLCOUNT PSLCOUNT))

  (IF (= SLTPOS NIL)
    (progn
      (SETQ SLTPOS (GETPOINT "\nShort Line Table Position:"))

        (SETVAR "CLAYER"  "Drafting" )


    			 ;box corners
			 (setq p10 (list (+ (car SLTPOS) 0)(+ (cadr SLTPOS) (* -2.5 th))))
			 (setq p11 (list (+ (car SLTPOS) (* 4 th))(+ (cadr SLTPOS)  0 )))
			 (setq p12 (list (+ (car SLTPOS) (* 13 th))(+ (cadr SLTPOS) (* -2.5 th))))
			 (setq p13 (list (+ (car SLTPOS) (* 21 th))(+ (cadr SLTPOS)  0 )))
			 
			 ;draw boxes
			 (command "rectangle" p10 p11)
			 (command "rectangle" p11 p12)
			 (command "rectangle" p12 p13)
			 
			 ;text insertion points
			 (setq p20 (list (+ (car SLTPOS) (* 2 th))(+ (cadr SLTPOS)  (* -1.25 th ))))
			 (setq p21 (list (+ (car SLTPOS) (* 8.5 th))(+ (cadr SLTPOS)  (* -1.25 th ))))
			 (setq p22 (list (+ (car SLTPOS) (* 17 th))(+ (cadr SLTPOS)  (* -1.25 th ))))
			 
			 ;create text
			 (command "text" "j" "mc" p20 th "90" "NUM")
			 (command "text" "j" "mc" p21 th "90" "BEARING")
			 (command "text" "j" "mc" p22 th "90" "DISTANCE")
			 ;reset pm box mark point
			 (setq SLTPOS p10)
    ));P&IF FIRST BOX MARK
  


    
  

  (SETQ COUNT 0)
(REPEAT (SSLENGTH LINES)
(SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ P1 (LIST (CAR P1) (CADR P1)));2DISE P1 TO GIVE 2D DISTANCE
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ LAYER (CDR(ASSOC 8 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ ANG (ANGLE P1 P2))
  (SETQ BEARING (ANGTOS ANG 1 4));REQUIRED FOR ELSE ROUND
  (setq bearing (vl-string-subst "d" (chr 176) bearing));added for BricsCAD using degrees instead of "d"
  (SETQ DIST (DISTANCE (LIST (CAR P1)(CADR P1)) P2));REQUIRED FOR ELSE ROUND


  (IF (= QROUND "YES")(PROGN

			(SETQ LLEN (DISTANCE (LIST (CAR P1)(CADR P1)) P2))

    ;ASSIGN ROUNDING FOR ANGLES BASED ON DISTANCE
    (IF (< LLEN MAXLEN1) (SETQ ROUND BRND1))
    (IF (AND (> LLEN MAXLEN1)(< LLEN MAXLEN2)) (SETQ ROUND BRND2))
    (IF (> LLEN MAXLEN2)(SETQ ROUND BRND3))

   
    ;GET ANGLE DELIMIETERS
    (SETQ SANG (ANGTOS ANG 1 4))
    (setq sang (vl-string-subst "d" (chr 176) sang));added for BricsCAD using degrees instead of "d"
    (setq CHRDPOS (vl-string-position 100 SANG 0))
    (setq MINPOS (vl-string-position 39 SANG 0))
    (setq SECPOS (vl-string-position 34 SANG 0))

    ;PARSE ANGLE
    (setq DEG  (atof (substr SANG 1  CHRDPOS )))
    (setq MINS  (atof (substr SANG (+ CHRDPOS 2)  (-(- MINPOS CHRDPOS)1))))
    (setq SEC  (atof (substr SANG (+ MINPOS 2)  (-(- SECPOS MINPOS )1))))

   
;ROUND ANGLE, NOTE SECONDS REMOVED
     (IF (and (= ROUND 60)(< SEC 30)) (SETQ SEC 0))
    (IF (and (= ROUND 60)(>= SEC 30)) (SETQ SEC 0
					    MINS (+ MINS 1)))	
    (IF (/= ROUND 60) (PROGN
			(SETQ SIP (FIX (/ SEC ROUND)))
			(SETQ SFP (- (/  SEC ROUND) SIP))
			(IF (>= SFP 0.5) (SETQ SIP (+ SIP 1)))
			(SETQ SEC (* SIP ROUND))
			)
      )

    ;ROUND ALL DISTANCES
    (IF (< LLEN DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> LLEN DISTMAX1)(< LLEN DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> LLEN DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ LLEN DROUND)))
    (SETQ LFP (- (/ LLEN DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ LLEN (* LIP DROUND))

    (SETQ LDIST (RTOS LLEN 2 3))
   
		 
    
;STRING ANGLES
    (SETQ DEG (RTOS DEG 2 0))
    (SETQ MINS (RTOS MINS 2 0))
    (SETQ SEC (RTOS SEC 2 0))
    
;INCREMENT IF SECONDS ROUNDED TO 60
    (IF (= SEC  "60")
      (PROGN
	(SETQ SEC "00")
	(SETQ MINS (RTOS (+ (ATOF MINS ) 1) 2 0))
	)
      )
;INCREMENT IF MINUTES ROUNDED TO 60
    (IF (= MINS "60")
      (PROGN
	(SETQ MINS "00")
	(SETQ DEG (RTOS (+ (ATOF DEG ) 1) 2 0))
	)
      )
;FIX IF INCREMENTING PUSHES DEG PAST 360    
    (IF (= DEG "360")(SETQ DEG "0"))
;ADD ZEROS TO SINGLE NUMBERS	
 (IF (= (STRLEN MINS) 1)(SETQ MINS (STRCAT "0" MINS)))
  (IF (= (STRLEN SEC) 1)(SETQ SEC (STRCAT "0" SEC)))

;TRUNCATE BEARINGS IF 00'S
  (IF (AND (= MINS "00") (= SEC "00")) (SETQ MINSS ""
					     SECS "")
        (SETQ MINSS(STRCAT MINS "'")
	  SECS (STRCAT SEC "\""))
	  )
    (IF (= SEC "00")(SETQ SECS ""
			    SEC ""))

			

    ;CONCATENATE BEARING
    (SETQ BEARING (STRCAT DEG "d" MINSS SECS ))

			(IF (or (/= sec "00")(/= MINS "00"))(SETQ DEG (STRCAT DEG ".")))

  (SETQ LBEARING (STRCAT DEG MINS SEC))
			
    

			);P&IF


(PROGN;ELSE

  

  (SETQ DPOS (vl-string-position 100 BEARING 0))
  (setq Wpos (vl-string-position 39 BEARING 0))
  (setq WWpos (vl-string-position 34 BEARING 0))

    (setq DEG (substr BEARING 1 Dpos))
      (setq MINS (substr BEARING (+ Dpos 2) (- (- WPOS DPOS) 1)))
      (setq SEC (substr BEARING (+ Wpos 2) (- (- WWpos Wpos) 1)))

  (IF (= (STRLEN MINS) 1)(SETQ MINS (STRCAT "0" MINS)))
  (IF (= (STRLEN SEC) 1)(SETQ SEC (STRCAT "0" SEC)))
  
  (IF (AND (= MINS "00") (= SEC "00")) (SETQ MINSS ""
					     SECS ""
					     MINS ""
					     SEC "")
    (SETQ MINSS(STRCAT MINS "'")
	  SECS (STRCAT SEC "\""))
	  	  )
  (IF (= SECS "00\"")(SETQ SECS ""
			   SEC ""))
  
  (SETQ BEARING (STRCAT DEG "d" MINSS SECS ))

  (IF (/= MINS "")(SETQ DEG (STRCAT DEG ".")))

  
  
  (SETQ LBEARING (STRCAT DEG MINS SEC))
  (SETQ LDIST (RTOS DIST 2 3))

  ));PELSE&IF

  (COMMAND "ERASE" EN "")
  (SETVAR "CLAYER" layer)
  (COMMAND "LINE" (trans P1 0 1)(trans  P2 0 1) "")
  
(SETQ BDINFO (STRCAT "azimuth=\"" lbearing "\" horizDistance=\"" ldist "\"/>"))
 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)


  (LBS)

  (SETQ COUNT (+ COUNT 1))
  (SETQ SLCOUNT (RTOS (+ (ATOF SLCOUNT) 1) 2 0))

  );R


      (SETVAR "CLAYER" prevlayer)

  );DEFUN




;-------------------------------------------------------------SHORT ARC TABLE-----------------------------------

(DEFUN C:XSC (/)

  (setq prevlayer (getvar "CLAYER"))
(SETQ COMMENT "")
 (SETQ LINES (SSGET  '((0 . "ARC"))))

  (IF (/= SLCOUNT NIL)  (SETQ PSLCOUNT SLCOUNT)(SETQ SLCOUNT ""
						     PSLCOUNT ""))
  (SETQ SLCOUNT (GETSTRING (STRCAT "\nShort Arc Starting Number (" SLCOUNT "):" )))
  (IF (= SLCOUNT "" )(SETQ SLCOUNT PSLCOUNT))

  (IF (= SLATPOS NIL)
    (progn
      (SETQ SLATPOS (GETPOINT "\nShort Arc Table Position:"))

        (SETVAR "CLAYER"  "Drafting" )


    			 ;box corners
			 (setq p10 (list (+ (car SLATPOS) 0)(+ (cadr SLATPOS) (* -2.5 th))))
			 (setq p11 (list (+ (car SLATPOS) (* 4 th))(+ (cadr SLATPOS)  0 )))
			 (setq p12 (list (+ (car SLATPOS) (* 13 th))(+ (cadr SLATPOS) (* -2.5 th))))
			 (setq p13 (list (+ (car SLATPOS) (* 21 th))(+ (cadr SLATPOS)  0 )))
                         (setq p14 (list (+ (car SLATPOS) (* 29 th))(+ (cadr SLATPOS)  (* -2.5 th ))))
                         (setq p15 (list (+ (car SLATPOS) (* 37 th))(+ (cadr SLATPOS)  0 )))
      
			 
			 ;draw boxes
			 (command "rectangle" p10 p11)
			 (command "rectangle" p11 p12)
			 (command "rectangle" p12 p13)
                         (command "rectangle" p13 p14)
                         (command "rectangle" p14 p15)
      
			 
			 ;text insertion points
			 (setq p20 (list (+ (car SLATPOS) (* 2 th))(+ (cadr SLATPOS)  (* -1.25 th ))))
			 (setq p21 (list (+ (car SLATPOS) (* 8.5 th))(+ (cadr SLATPOS)  (* -1.25 th ))))
			 (setq p22 (list (+ (car SLATPOS) (* 17 th))(+ (cadr SLATPOS)  (* -1.25 th ))))
                         (setq p23 (list (+ (car SLATPOS) (* 25 th))(+ (cadr SLATPOS)  (* -1.25 th ))))
                         (setq p24 (list (+ (car SLATPOS) (* 33 th))(+ (cadr SLATPOS)  (* -1.25 th ))))
			 
			 ;create text
			 (command "text" "j" "mc" p20 th "90" "NUM")
			 (command "text" "j" "mc" p21 th "90" "BEARING")
			 (command "text" "j" "mc" p22 th "90" "DISTANCE")
                         (command "text" "j" "mc" p23 th "90" "ARC")
                         (command "text" "j" "mc" p24 th "90" "RADIUS")
			 ;reset pm box mark point
			 (setq SLATPOS p10)
    ));P&IF FIRST BOX MARK
  


    
  
  (SETQ COUNT 0)
(REPEAT (SSLENGTH LINES)
(SETQ CP (CDR(ASSOC 10 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ RADIUS (CDR(ASSOC 40 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ LAYER (CDR(ASSOC 8 (ENTGET (SSNAME LINES COUNT)))))

  (SETQ ANG1 (CDR(ASSOC 50 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ ANG2 (CDR(ASSOC 51 (ENTGET (SSNAME LINES COUNT)))))

  (SETQ P1 (POLAR CP ANG1 RADIUS))
  (SETQ P2 (POLAR CP ANG2 RADIUS))
  (SETQ ANG (ANGLE P1 P2))
  
(SETQ CURVEROT "ccw")
  ;calc curve midpoint
  (setq a1 (angle CP p1))
  (setq a2 (angle CP p2))
  (if (= curverot "ccw")(setq da (- a2 a1))(setq da (- a1 a2)))
  (if (< da 0)(setq da (+ da (* 2 pi))))
    (SETQ DA (/ DA 2))
    (IF (= CURVEROT "ccw")(setq midb (+ a1 da))(setq midb (+ a2 da)))
  (setq amp (polar CP midb radius))

  (SETQ BEARING (ANGTOS ANG 1 4))
  (setq bearing (vl-string-subst "d" (chr 176) bearing));added for BricsCAD changes degrees to "d"
  (SETQ DIST (DISTANCE (LIST (CAR P1)(CADR P1))P2))

 (IF (= QROUND "YES")(PROGN

			(SETQ LLEN (DISTANCE (LIST (CAR P1)(CADR P1)) P2))

    ;ASSIGN ROUNDING FOR ANGLES BASED ON DISTANCE
    (IF (< LLEN MAXLEN1) (SETQ ROUND BRND1))
    (IF (AND (> LLEN MAXLEN1)(< LLEN MAXLEN2)) (SETQ ROUND BRND2))
    (IF (> LLEN MAXLEN2)(SETQ ROUND BRND3))

   
    ;GET ANGLE DELIMIETERS
    (SETQ SANG (ANGTOS ANG 1 4))
    (setq sang (vl-string-subst "d" (chr 176) sang));added for BricsCAD changes degrees to "d"
    (setq CHRDPOS (vl-string-position 100 SANG 0))
    (setq MINPOS (vl-string-position 39 SANG 0))
    (setq SECPOS (vl-string-position 34 SANG 0))

    ;PARSE ANGLE
    (setq DEG  (atof (substr SANG 1  CHRDPOS )))
    (setq MINS  (atof (substr SANG (+ CHRDPOS 2)  (-(- MINPOS CHRDPOS)1))))
    (setq SEC  (atof (substr SANG (+ MINPOS 2)  (-(- SECPOS MINPOS )1))))

   
;ROUND ANGLE, NOTE SECONDS REMOVED
     (IF (and (= ROUND 60)(< SEC 30)) (SETQ SEC 0))
    (IF (and (= ROUND 60)(>= SEC 30)) (SETQ SEC 0
					    MINS (+ MINS 1)))	
    (IF (/= ROUND 60) (PROGN
			(SETQ SIP (FIX (/ SEC ROUND)))
			(SETQ SFP (- (/  SEC ROUND) SIP))
			(IF (>= SFP 0.5) (SETQ SIP (+ SIP 1)))
			(SETQ SEC (* SIP ROUND))
			)
      )

    ;ROUND ALL DISTANCES TO 5MM
    (IF (< LLEN DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> LLEN DISTMAX1)(< LLEN DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> LLEN DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ LLEN DROUND)))
    (SETQ LFP (- (/ LLEN DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ LLEN (* LIP DROUND))
      
   
		 
    
;STRING ANGLES
    (SETQ DEG (RTOS DEG 2 0))
    (SETQ MINS (RTOS MINS 2 0))
    (SETQ SEC (RTOS SEC 2 0))
    
;INCREMENT IF SECONDS ROUNDED TO 60
    (IF (= SEC  "60")
      (PROGN
	(SETQ SEC "00")
	(SETQ MINS (RTOS (+ (ATOF MINS ) 1) 2 0))
	)
      )
;INCREMENT IF MINUTES ROUNDED TO 60
    (IF (= MINS "60")
      (PROGN
	(SETQ MINS "00")
	(SETQ DEG (RTOS (+ (ATOF DEG ) 1) 2 0))
	)
      )
;FIX IF INCREMENTING PUSHES DEG PAST 360    
    (IF (= DEG "360")(SETQ DEG "0"))
;ADD ZEROS TO SINGLE NUMBERS	
 (IF (= (STRLEN MINS) 1)(SETQ MINS (STRCAT "0" MINS)))
  (IF (= (STRLEN SEC) 1)(SETQ SEC (STRCAT "0" SEC)))

;TRUNCATE BEARINGS IF 00'S
  (IF (AND (= MINS "00") (= SEC "00")) (SETQ MINSS ""
					     MINS ""
					     SECS ""
					     SEC "")
    ;ELSE
        (SETQ MINSS(STRCAT MINS "'")
	  SECS (STRCAT SEC "\""))
	  )
    (IF (= SECS "00\"")(SETQ SECS ""
			     SEC ""))

			

    ;CONCATENATE BEARING
    (SETQ BEARING (STRCAT DEG "d" MINSS SECS ))

			(IF (or (/= sec "")(/= MINS ""))(SETQ DEG (STRCAT DEG ".")))

  (SETQ LBEARING (STRCAT DEG MINS SEC))
			
    

			);P&IF ROUNDING


(PROGN;ELSE

  

  (SETQ DPOS (vl-string-position 100 BEARING 0))
  (setq Wpos (vl-string-position 39 BEARING 0))
  (setq WWpos (vl-string-position 34 BEARING 0))

    (setq DEG (substr BEARING 1 Dpos))
      (setq MINS (substr BEARING (+ Dpos 2) (- (- WPOS DPOS) 1)))
      (setq SEC (substr BEARING (+ Wpos 2) (- (- WWpos Wpos) 1)))

  (IF (= (STRLEN MINS) 1)(SETQ MINS (STRCAT "0" MINS)))
  (IF (= (STRLEN SEC) 1)(SETQ SEC (STRCAT "0" SEC)))
  
  (IF (AND (= MINS "00") (= SEC "00")) (SETQ MINSS ""
					     SECS ""
					     MINS ""
					     SEC "")
    (SETQ MINSS(STRCAT MINS "'")
	  SECS (STRCAT SEC "\""))
	  )
  (IF (= SECS "00\"")(SETQ SECS ""
			   SEC ""))
  
  (SETQ BEARING (STRCAT DEG "d" MINSS SECS ))

  	(IF (or (/= sec "")(/= MINS ""))(SETQ DEG (STRCAT DEG ".")))

  (SETQ LBEARING (STRCAT DEG MINS SEC))
  (SETQ LDIST (RTOS DIST 2 3))

  ));PELSE&IF




   (SETQ MAST (SQRT (- (*  RADIUS  RADIUS) (* (/  DIST 2)(/ DIST 2 )))))





  
  ;(SETQ O (* 2 (ATAN (/ (/  DIST 2) MAST))))
  (SETQ O (- ANG2 ANG1))
  (IF (< O 0) (SETQ O (+ O (* PI 2))))
  	   (setq arclength (rtos ( *  radius O) 2 3))



  
  (setq digchaz (angle p1 p2))
    (SETQ O1 (* 2 (ATAN (/ (/ (distance p1 p2) 2) MAST))))
	    (setq remhalfO  (- (* 0.5 pi) (/ O1 2)))
	    (if (and (= curverot "ccw") (<= (atof arclength) (* pi  radius)))(setq raybearing (+  digchaz  remhalfO)))
	    (IF (and (= curverot "cw") (<= (atof arclength) (* pi  radius)))(setq raybearing (-  digchaz  remhalfO)))
	    (IF (and (= curverot "ccw") (> (atof arclength) (* pi  radius)))(setq raybearing (-  digchaz  remhalfO)))
	    (if (and (= curverot "cw") (> (atof arclength) (* pi  radius)))(setq raybearing (+  digchaz  remhalfO)))

  

  
  
(if (= qround "YES")(progn 
 ;ROUND ALL DISTANCES TO 5MM
    (SETQ LIP (FIX (/ (atof ARCLENGTH) 0.005)))
    (SETQ LFP (- (/ (atof ARCLENGTH) 0.005) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ ARCLENGTH (* LIP 0.005))
    
    (SETQ ARCLENGTH (RTOS ARCLENGTH 2 3))
    ))

  (if (= qround "YES")(progn 
 ;ROUND ALL DISTANCES TO 5MM
    (SETQ LIP (FIX (/ radius 0.005)))
    (SETQ LFP (- (/ radius 0.005) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ radius(* LIP 0.005))
    
   
    ))
  


  

  (COMMAND "ERASE" EN "")
  (SETVAR "CLAYER" layer)
  (COMMAND "ARC" "c" (TRANS CP 0 1) (TRANS P1 0 1) (TRANS P2 0 1 ))

  
      (if (/= comment "")(setq ocomment (strcat "><FieldNote>\"" comment "\"</FieldNote></ReducedArcObservation>"))(setq ocomment "/>"))
(SETQ BDINFO (STRCAT "chordAzimuth=\"" Lbearing "\" length=\"" arclength "\" radius=\"" (RTOS RADIUS 2 3)  "\" rot=\"ccw\"  arcType=\"Measured\"" ocomment))
 
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

      (setq tp1 p1)
      (setq tp2 p2)
   
      (setq lradius (rtos radius 2 3))
(lbSA);label line if not already labelled;label arc using function



  (SETQ COUNT (+ COUNT 1))
    (SETQ SLCOUNT (RTOS (+ (ATOF SLCOUNT) 1) 2 0))

  );R


      (SETVAR "CLAYER" prevlayer)

  );DEFUN






;--------------------------------------------------------------LINE LABELLER FUCNTION---------------------------
	    
(defun lba (/)

  
  (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
   (setq tp1 (CDR(ASSOC 10 sentlist)))
   (setq tp2 (CDR(ASSOC 11 sentlist)))

       
    (SETQ AANG (ANGLE tP1 tP2))
    (SETQ CP1 (TRANS  tP1 0 1))
    (SETQ CP2 (TRANS  tP2 0 1))
    (SETQ ANG (ANGLE CP1 CP2))
    (SETQ DANG (* ANG (/ 180 PI)))

    (IF (AND (> DANG 90) (<= DANG 270)) (PROGN
					 (SETQ CP1 (TRANS tP2 0 1))
					 (SETQ CP2 (TRANS tP1 0 1))
					 (SETQ ANG (ANGLE CP1 CP2))
                                         (SETQ DANG (* ANG (/ 180 PI)))
					 )
      )

  ;REDUCE SIZE ON SMALL LINES

  
  (SETQ THP TH)
  (IF (AND (/= ldist "")(< (ATOF LDIST) (* 10 th)) (<  (* (ATOF LDIST) 0.1) TH)(= ATHR "Y"))(SETQ TH (* (ATOF LDIST) 0.1)))
  
  
    (SETQ MPE (/ (+ (CAR CP1 ) (CAR CP2)) 2))
    (SETQ MPN (/ (+ (CADR CP1 ) (CADR CP2)) 2))
    (SETQ MP (LIST MPE MPN))
    (SETQ BPOS (POLAR MP (+ ANG (* 0.5 PI)) TH))
    (SETQ DPOS (POLAR MP (- ANG (* 0.5 PI)) TH))
    (SETQ BTS (vl-string-subst  "°" "d" bearing))

  (setq ulb "")
  (setq uld "")

  (if (and (= distancetype  " distanceType=\"Adopt Dimension\" ")(= comment ""))(setq comment  "AD"))
  (if (and (= distancetype  " distanceType=\"Computed\" ")(= comment ""))(setq comment  "COMP"))
  (if (= distancetype  " distanceType=\"Derived\" ")(setq uld "%%u"))
  (if (= azimuthtype  " azimuthType=\"Derived\" ")(setq ulb "%%u"))
  
  (if (/= comment "")(setq comment (strcat " "comment)))

  (setq prevlayer (getvar "CLAYER"))
  (if (or (= prevlayer "Traverse")(= prevlayer "Sideshot"))  (SETVAR "CLAYER"  "Drafting AFR" )(SETVAR "CLAYER"  "Drafting" ))
    (COMMAND "TEXT" "J" "MC" BPOS TH (ANGTOS ANG 1 4) (strcat ulb BTS))
    (if (/= ldist "") (COMMAND "TEXT" "J" "MC" DPOS TH (ANGTOS ANG 1 4) (strcat uld ldist (strcase comment))))
(if (= prevlayer "Boundary Extinguished")(progn
					   (SETVAR "CLAYER"  "Drafting AFR" )
					   (COMMAND "TEXT" "J" "MC" BPOS TH (ANGTOS ANG 1 4) (strcat ulb BTS))
    (if (/= ldist "") (COMMAND "TEXT" "J" "MC" DPOS TH (ANGTOS ANG 1 4) (strcat uld ldist (strcase comment))))
					   ))
	


  (if ( = labelyeolddist 1)(progn
			    
(SETQ YODPOS (POLAR MP (- ANG (* 0.5 PI)) (* 2.5 TH)))
  (setq textstyle (getvar "textstyle"))
(SETQ textfont (ENTGET (tblobjname "style" textstyle)))
(setq theElist (subst (cons 50 (* 20 (/ PI 180)))(assoc 50 theElist) textfont));make ye old distances slanty
(entmod theElist)
  (COMMAND "TEXT" "J" "MC" YODPOS TH (ANGTOS ANG 1 4) prevdist )
  (setq theElist (subst (cons 50 0)(assoc 50 theElist) textfont));set slanty back to straight
(entmod theElist)
(setq labelyeolddist 0)
		    
			    ));if yeolddist
   (SETQ TH THP)


  
    )


;--------------------------------------------------------------SHORT LINE LABELLER FUCNTION---------------------------
	    
(defun lbs (/)

  
  (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
   (setq tp1 (CDR(ASSOC 10 sentlist)))
   (setq tp2 (CDR(ASSOC 11 sentlist)))

       
    (SETQ AANG (ANGLE tP1 tP2))
    (SETQ CP1 (TRANS  tP1 0 1))
    (SETQ CP2 (TRANS  tP2 0 1))
    (SETQ ANG (ANGLE CP1 CP2))
    (SETQ DANG (* ANG (/ 180 PI)))

    (IF (AND (> DANG 90) (<= DANG 270)) (PROGN
					 (SETQ CP1 (TRANS tP2 0 1))
					 (SETQ CP2 (TRANS tP1 0 1))
					 (SETQ ANG (ANGLE CP1 CP2))
                                         (SETQ DANG (* ANG (/ 180 PI)))
					 )
      )

  ;REDUCE SIZE ON SMALL LINES

  
  (SETQ THP TH)
  ;(IF (AND (< (ATOF LDIST) (* 10 th)) (<  (* (ATOF LDIST) 0.1) TH)(= ATHR "Y"))(SETQ TH (* (ATOF LDIST) 0.1)))
  
  
    (SETQ MPE (/ (+ (CAR CP1 ) (CAR CP2)) 2))
    (SETQ MPN (/ (+ (CADR CP1 ) (CADR CP2)) 2))
    (SETQ MP (LIST MPE MPN))
    (SETQ BPOS (POLAR MP (+ ANG (* 0.5 PI)) (* TH 1.2)))
    (SETQ DPOS (POLAR MP (- ANG (* 0.5 PI)) (* TH 1.2)))
    (SETQ BTS (vl-string-subst  "°" "d" bearing))

  (if (/= comment "")(setq comment (strcat " "comment)))

  (setq prevlayer (getvar "CLAYER"))
  (SETVAR "CLAYER"  "Drafting" )
    (COMMAND "TEXT" "J" "MC" BPOS TH "90" SLCOUNT)
  (COMMAND "CIRCLE" BPOS TH)
    

	 (SETQ TH THP)


  
    			 ;box corners
			 (setq p10 (list (+ (car SLTPOS) 0)(+ (cadr SLTPOS) (* -2.5 th))))
			 (setq p11 (list (+ (car SLTPOS) (* 4 th))(+ (cadr SLTPOS)  0 )))
			 (setq p12 (list (+ (car SLTPOS) (* 13 th))(+ (cadr SLTPOS) (* -2.5 th))))
			 (setq p13 (list (+ (car SLTPOS) (* 21 th))(+ (cadr SLTPOS)  0 )))
			 
			 ;draw boxes
			 (command "rectangle" p10 p11)
			 (command "rectangle" p11 p12)
			 (command "rectangle" p12 p13)
			 
			 ;text insertion points
			 (setq p20 (list (+ (car SLTPOS) (* 2 th))(+ (cadr SLTPOS)  (* -1.25 th ))))
			 (setq p21 (list (+ (car SLTPOS) (* 8.5 th))(+ (cadr SLTPOS)  (* -1.25 th ))))
			 (setq p22 (list (+ (car SLTPOS) (* 17 th))(+ (cadr SLTPOS)  (* -1.25 th ))))
			 
			 ;create text
			 (command "text" "j" "mc" p20 th "90" SLCOUNT)
			 (command "text" "j" "mc" p21 th "90" BTS)
			 (command "text" "j" "mc" p22 th "90" (STRCAT LDIST (strcase COMMENT)))
			 ;reset pm box mark point
			 (setq SLTPOS p10)



  
    
    )



;--------------------------------------------------------------ARC LABELLER FUNCTION--------------------------

(DEFUN LBARC (/)

     
    (SETQ AANG (ANGLE tP1 tP2))
    (SETQ CP1 (TRANS  tP1 0 1))
    (SETQ CP2 (TRANS  tP2 0 1))
    (SETQ ANG (ANGLE CP1 CP2))
    (SETQ DANG (* ANG (/ 180 PI)))

    (IF (AND (> DANG 90) (<= DANG 270)) (PROGN
					 (SETQ CP1 (TRANS tP2 0 1))
					 (SETQ CP2 (TRANS tP1 0 1))
					 (SETQ ANG (ANGLE CP1 CP2))
                                         (SETQ DANG (* ANG (/ 180 PI)))
					 )
      )

  ;REDUCE SIZE ON SMALL LINES

  
  (SETQ THP TH)
  (IF (AND (< (ATOF LDIST) (* 10 th)) (<  (* (ATOF LDIST) 0.1) TH)(= ATHR "Y"))(SETQ TH (* (ATOF LDIST) 0.1)))
  
  
    (SETQ MP (TRANS AMP 0 1))
    

  (IF (or (AND (OR (>= AANG (* 1.5 PI))(<= AANG (* 0.5 PI)))( = curverot "ccw"))(AND (<= AANG (* 1.5 PI))(> AANG (* 0.5 PI))( = curverot "cw")))
    (progn
      (setq MP (POLAR MP (+ ANG (* 0.5 PI)) (* TH 6.7)))
      )
    
    )
    (SETQ BPOS (POLAR MP (- ANG (* 0.5 PI))  TH))
    (SETQ DPOS (POLAR MP (- ANG (* 0.5 PI)) (* 2.5 TH)))
    (SETQ APOS (POLAR MP (- ANG (* 0.5 PI)) (* 3.9 TH)))
    (SETQ RPOS (POLAR MP (- ANG (* 0.5 PI)) (* 5.3 TH)))
 
    (SETQ BTS (vl-string-subst  "°" "d" bearing))
  
   (setq ul "")
  (if (and (= arctype " arcType=\"Adopt Dimension\" ")(= comment ""))(setq comment  "AD"))
  (if (and (= arctype " arcType=\"Computed\" ")(= comment ""))(setq comment  "COMP"))
  (if (= arctype " arcType=\"Derived\" ")(setq ul "%%u"))

  (if (/= comment "")(setq comment (strcat " " comment)))
  
  (setq prevlayer (getvar "CLAYER"))
  (if (or (= prevlayer "Traverse")(= prevlayer "Sideshot"))  (SETVAR "CLAYER"  "Drafting AFR" )(SETVAR "CLAYER"  "Drafting" ))
  (SETVAR "CLAYER" "Drafting")
    (COMMAND "TEXT" "J" "MC" BPOS TH (ANGTOS ANG 1 4) (STRCAT UL BTS))
    (COMMAND "TEXT" "J" "MC" DPOS TH (ANGTOS ANG 1 4) (strcat UL "CH" ldist (strcase comment)))
    (COMMAND "TEXT" "J" "MC" APOS TH (ANGTOS ANG 1 4) (strcat UL "ARC" arclength))
    (COMMAND "TEXT" "J" "MC" RPOS TH (ANGTOS ANG 1 4) (strcat UL "RAD" lradius))

  (if (= prevlayer "Boundary Extinguished")(progn
					     (SETVAR "CLAYER" "Drafting AFR")
    (COMMAND "TEXT" "J" "MC" BPOS TH (ANGTOS ANG 1 4) (STRCAT UL BTS))
    (COMMAND "TEXT" "J" "MC" DPOS TH (ANGTOS ANG 1 4) (strcat UL "CH" ldist (strcase comment)))
    (COMMAND "TEXT" "J" "MC" APOS TH (ANGTOS ANG 1 4) (strcat UL "ARC" arclength))
    (COMMAND "TEXT" "J" "MC" RPOS TH (ANGTOS ANG 1 4) (strcat UL "RAD" lradius))
					     ))


	 (SETQ TH THP)





   ;end of arc labeller
)





;--------------------------------------------------------------SHORT ARC LABELLER FUNCTION--------------------------

(DEFUN LBSA (/)

  (SETQ AANG (ANGLE tP1 tP2))
    (SETQ CP1 (TRANS  tP1 0 1))
    (SETQ CP2 (TRANS  tP2 0 1))
    (SETQ ANG (ANGLE CP1 CP2))
    (SETQ DANG (* ANG (/ 180 PI)))

    (IF (AND (> DANG 90) (<= DANG 270)) (PROGN
					 (SETQ CP1 (TRANS tP2 0 1))
					 (SETQ CP2 (TRANS tP1 0 1))
					 (SETQ ANG (ANGLE CP1 CP2))
                                         (SETQ DANG (* ANG (/ 180 PI)))
					 )
      )

  ;REDUCE SIZE ON SMALL LINES

  
  (SETQ THP TH)
  (IF (AND (< (ATOF LDIST) (* 10 th)) (<  (* (ATOF LDIST) 0.1) TH)(= ATHR "Y"))(SETQ TH (* (ATOF LDIST) 0.1)))
  
  
    (SETQ MP (TRANS AMP 0 1))
    

  (IF (or (AND (OR (>= AANG (* 1.5 PI))(<= AANG (* 0.5 PI)))( = curverot "ccw"))(AND (<= AANG (* 1.5 PI))(> AANG (* 0.5 PI))( = curverot "cw")))
    (progn
      (setq MP (POLAR MP (+ ANG (* 0.5 PI)) (* TH 6.7)))
      )
    
    )
    (SETQ BPOS (POLAR MP (- ANG (* 0.5 PI))  TH))
    (SETQ DPOS (POLAR MP (- ANG (* 0.5 PI)) (* 2.5 TH)))
    (SETQ APOS (POLAR MP (- ANG (* 0.5 PI)) (* 3.9 TH)))
    (SETQ RPOS (POLAR MP (- ANG (* 0.5 PI)) (* 5.3 TH)))
 
    (SETQ BTS (vl-string-subst  "°" "d" bearing))
  
  (setq prevlayer (getvar "CLAYER"))
  (SETVAR "CLAYER"  "Drafting" )
    (COMMAND "TEXT" "J" "MC" BPOS TH "90" SLCOUNT)
  (COMMAND "CIRCLE" BPOS TH)
    

	 (SETQ TH THP)
     
  
    			 ;box corners
			 (setq p10 (list (+ (car SLATPOS) 0)(+ (cadr SLATPOS) (* -2.5 th))))
			 (setq p11 (list (+ (car SLATPOS) (* 4 th))(+ (cadr SLATPOS)  0 )))
			 (setq p12 (list (+ (car SLATPOS) (* 13 th))(+ (cadr SLATPOS) (* -2.5 th))))
			 (setq p13 (list (+ (car SLATPOS) (* 21 th))(+ (cadr SLATPOS)  0 )))
                         (setq p14 (list (+ (car SLATPOS) (* 29 th))(+ (cadr SLATPOS)  (* -2.5 th ))))
                         (setq p15 (list (+ (car SLATPOS) (* 37 th))(+ (cadr SLATPOS)  0 )))
      
			 
			 ;draw boxes
			 (command "rectangle" p10 p11)
			 (command "rectangle" p11 p12)
			 (command "rectangle" p12 p13)
                         (command "rectangle" p13 p14)
                         (command "rectangle" p14 p15)
      
			 
			 ;text insertion points
			 (setq p20 (list (+ (car SLATPOS) (* 2 th))(+ (cadr SLATPOS)  (* -1.25 th ))))
			 (setq p21 (list (+ (car SLATPOS) (* 8.5 th))(+ (cadr SLATPOS)  (* -1.25 th ))))
			 (setq p22 (list (+ (car SLATPOS) (* 17 th))(+ (cadr SLATPOS)  (* -1.25 th ))))
                         (setq p23 (list (+ (car SLATPOS) (* 25 th))(+ (cadr SLATPOS)  (* -1.25 th ))))
                         (setq p24 (list (+ (car SLATPOS) (* 33 th))(+ (cadr SLATPOS)  (* -1.25 th ))))
			 
			 ;create text
			 (command "text" "j" "mc" p20 th "90" SLCOUNT)
			 (command "text" "j" "mc" p21 th "90" BTS)
			 (command "text" "j" "mc" p22 th "90" LDIST)
                         (command "text" "j" "mc" p23 th "90" ARCLENGTH)
                         (command "text" "j" "mc" p24 th "90" LRADIUS)
			 ;reset pm box mark point
			 (setq SLATPOS p10)

  
   
   ;end of arc labeller
)


;------------------------------------------------------------------Label Corner Mark-----------------------------------------------------
(defun lcm (/)

   ;draw monument info
  ;check for no values and replace with "none"
  (setq rmtypet rmtype)
  (if (= rmrefdp "none")(setq rmrefdp ""))
  (if (/= rmrefdp "")(setq rmrefdp (strcat "(" rmrefdp ")")))
  (if (= rmcondition "none")(setq rmcondition ""))
  (if (/= rmstate "")(setq rmstatet (strcat " " rmstate))(setq rmstatet ""))
  (IF (= rmcondition "Placed") (setq rmcondition " Pl"))
  (if (= rmcondition "Found") (setq rmcondition " Fd"))
  (if (/= rmcondition "")(setq rmconditiont (strcat  "" rmcondition  ))(setq rmconditiont ""))
  (if (or (= rmcomment "Reference point")(= rmcomment "Reference Mark")(= rmcomment "RM"))(setq rmornot "RM "
												rmcomment "")
     (setq rmornot ""))
  (if (and (= rmtype "Peg")(= rmstate "New"))(setq rmtypet "NP"))
  (if (and (= rmtype "Peg")(= rmstate "Existing"))(setq rmtypet "OP"))
  (if (and (= rmtype "Peg and Trench")(= rmstate "New"))(setq rmtypet "NPT"))
  (if (and (= rmtype "Peg and Trench")(= rmstate "Existing"))(setq rmtypet "OPT"))
  (if (and (= rmtype "G.I. Pipe")(= rmstate "Existing"))(setq rmtypet "GIP"))
  (if (= rmtype "G.I. Nail")(setq rmtypet "GIN"))
  (IF (= rmtype "Other" )(setq rmtypet ""))
  

				     
  ;(if (/= rmcomment "")(setq rmcomment (strcat rmcomment " ")))
    (SETQ AANG (/ pi 2))
  ;(if (and (= rmtype "")(or (= "Gone" rmstate)(= rmstate "Not Found")))(setq rmtype "RM"));trick for RM gone having no type




  (if (/= rmcomment "")(setq rmcommentt (strcat  " " rmcomment  ))(setq rmcommentt ""))

  
     (setq 1text (strcat rmornot (strcase rmtypet)  (strcase rmconditiont) (strcase rmcommentt)))


  
	   (setq rang (- aang (* 0.25 pi)))
(IF (AND (> rang  (* 0.5 pi)) (< rang (* 1.5 pi)))(progn ;if text is upsidedown reverse justification and rotation
					  (setq rang (+ rang pi))
					  (setq just "TR")
    (SETQ 1POS (POLAR P1 (- AANG (* 0.5 PI)) (* TH 3)))
					  
					  );p
	 (progn ;else normal 
	 (setq just "BL")
	  (SETQ 1POS (POLAR P1 AANG (* TH 2)))
    (SETQ 1POS (POLAR 1POS (- AANG (* 0.5 PI)) TH))
	 (SETQ 2POS (POLAR P1 AANG TH))
    (SETQ 2POS (POLAR 2POS (- AANG (* 0.5 PI)) (* TH 2)))
   
    	 );p else
	 );if

  (if (= rmrefdp "")(setq 1pos 2pos)) 
  (SETVAR "CLAYER"  "Drafting AFR" )
    (COMMAND "TEXT" "J" just 1POS TH (ANGTOS rANG 1 4) 1TEXT)
  (IF (/= rmrefdp "")(COMMAND "TEXT" "J" just 2POS TH (ANGTOS rANG 1 4) rmrefdp))

  
(IF (= rmtype "SSM (Standard Survey Mark)")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Drill Hole with Wings")(command "insert" "VDHW" "_S" TH P1 "0"))
(IF (= rmtype "Chisel Cut")(command "insert" "VETCH" "_S" TH P1 "0"))
(IF (= rmtype "Peg")(command "insert" "VPEG" "_S" TH P1  "0"))
(IF (= rmtype "Peg and Trench")(command "insert" "VPEG" "_S" TH P1 "0"))
(IF (= rmtype "Bolt")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Cross Head Nail")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Deep Driven Rod")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Drill Hole")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Dumpy")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "G.I. Nail")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Nail in Peg")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Nail in Rail")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Pin")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "G.I. Pipe")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Rivet")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Rod")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Reference Tree")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Screw")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Spike")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Survey Nail")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Other")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Nail in Join")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Tree")(command "insert" "VRM" "_S" TH P1 "0"))
(IF (= rmtype "Round Post")(command "insert" "VRPOST" "_S" TH P1 "0"))
(IF (= rmtype "Square Post")(command "insert" "VSPOST" "_S" TH P1 "0"))
(IF (= rmtype "Split Post")(command "insert" "VSPOST" "_S" TH P1 "0"))
(IF (= rmtype "Star Picket")(command "insert" "VSTPK" "_S" TH P1 "0"))



  

  
  )



;------------------------------------------------------------------Easement Counter---------------------------------------------------------------------
(defun easecount (/)

(setq easelist (list))
  (setq easelegend (list))
(IF (/= (setq lots (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Lot Definitions")))) nil)(progn

							     
 (setq count 0)
  (repeat (sslength lots)
  
    (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))
    

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	     (IF (/= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))

    (if (/= (setq stringpos (vl-string-search "Parcel name" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 13)))(setq pclname (substr xdatai (+ stringpos 14) (-(- wwpos 1)(+ stringpos 12)))))(setq pclname ""))

	     
	    
    (if (= (substr pclname 1 1) "E")
      (progn
	(setq easelist (append easelist (list (rtos (+ (atof (substr pclname 2 50)) 10000) 2 0))))
	(if (/= (setq stringpos (vl-string-search "desc" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq pcldesc (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq pcldesc ""))
	(setq sortingname (rtos (+ (atof (substr pclname 2 50)) 10000) 2 0))
	    (setq easelegend (append easelegend (list (strcat pcldesc "~" sortingname))))
	));p&if E
      
	
			       
	      
    
    ));if xdata exists
    (setq count (+ count 1))
    );repeat

	       
 (if (> (length easelist) 0)(progn;if easements exist
			       
 (setq easelist  (VL-SORT easelist '<))
 (setq easelegend  (VL-SORT easelegend '<))
 (setq easecounter (- (+  (atof (nth (- (length easelist)1) easelist)) 1) 10000))
 )
   (setq easecounter 1);else
   );easements exist
	       )
(setq easecounter 1);if not lots exist yet defult to 1
  );if lots exist
 
 )

	  
    
   
    

;----------------------------------------------------------------;CHOOSE FROM BOX FUNCTION--------------------------------


    ;;; defun

(defun dbox ()

  
   

  (setq dcl_id (load_dialog "Landxml.dcl"))
  (if (not (new_dialog "landxml" dcl_id))
    (exit)
  )

      (if ( = workingselnum nil)(setq workingselnum "0"))

	      (set_tile "selections" workingselnum)

   

 
  
  (start_list "selections")
  (mapcar 'add_list names)
  (end_list)
  (set_tile "selections" workingselnum)
  
  (action_tile
    "accept"
    (strcat
      "(progn (setq pick (atoi (get_tile \"selections\")))"
      ;"(setq table (atoi (get_tile \"cfs\")))"
      "(setq click \"Accept\")"
      "(done_dialog)(setq userclick T))"
    )
  )

;;;action_tile
  (action_tile "cancel"
    (strcat
      "(done_dialog)(setq userclick nil)"
       
      )
    )
;;;action_tile "cancel"
  (start_dialog)
  (unload_dialog dcl_id)


  (setq returntype (nth pick names))
(setq workingselnum (rtos pick 2 0))
  
);defun

   ;;; defun
;----------------------------------------------------------------;edit Xdata--------------------------------

(defun C:XDE ()

  
   (setq en (car (entsel "\nSelect Object:")))
  (setq count 0)
  
    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	   (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     
	      (princ (strcat "\nObject has no xdata"))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))

    

  (setq dcl_id (load_dialog "Landxml.dcl"))
  (if (not (new_dialog "XDE" dcl_id))
    (exit)
  )

     

	      (set_tile "xdata" xdatai)

 
  
  (action_tile
    "accept"
    (strcat
      "(setq xdatai (get_tile \"xdata\"))"
      ;"(setq table (atoi (get_tile \"cfs\")))"
      "(setq click \"Accept\")"
      "(done_dialog)(setq userclick T))"
    )
  )

;;;action_tile
  (action_tile "cancel"
    (strcat
      "(done_dialog)(setq userclick nil)"
       
      )
    )
;;;action_tile "cancel"
  (start_dialog)
  (unload_dialog dcl_id)

  (SETQ SENTLIST (ENTGET EN))
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 xdatai)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
 
  
);defun

(defun linereader (/)
  ;

;read the line			   
   (setq linetext (read-line xmlfile))

  ;remove tabs
(while (/=  (setq sybpos (vl-string-search "\t" linetext sybpos )) nil)
		(setq linetext (vl-string-subst "" "\t"  linetext 0)))
  
  
		     

  
  ;check for comment
  (while (vl-string-search "<!--" linetext )(progn
					   (while (= (vl-string-search "-->" linetext ) nil)(setq linetext (read-line xmlfile)))
					   (setq remstring (substr linetext (+ (vl-string-search "-->" linetext ) 4) 1000))
					   (if (= remstring"")(setq linetext (read-line xmlfile))(setq linetext remstring)
					       
					       )
					   
  
   ))
  )


;---------------------------------------------------------------FILEFINDER FOLDERS-----------------------------------

       
(defun findmyfile (/)

   (setq cdalf "YXWVUTSRQPNMLKJHGFEDCBA")

  (setq textstring (getstring "\nPlan Number:"))

(setq ref "")
  (setq cchkdigit "")

    (setq 3rdnum (substr textstring 3 1))

	       (setq subpos 3)
	       (setq refpos 3)

  
  (setq prefix (strcase(substr textstring 1 2)))
  
	       
	       (if (and (>= (ascii 3rdnum) 48)(<= (ascii 3rdnum) 57))
		 (progn
		  	       
	       
	       (while (and
			(>= (ascii (substr textstring subpos 1)) 48)
			(<= (ascii (substr textstring subpos 1)) 57)
			(/= subpos   (+ (strlen textstring)1  ))
			)
		 (progn
		 (setq ref (strcat ref (substr textstring subpos 1)))
		 (setq subpos (+ subpos 1))
		 ));p&w next charater is number

	       (while (< (strlen ref ) 6)(setq ref (strcat "0" ref)))

	       (if (or (= prefix "PS")
		       (= prefix "TP")
		       (= prefix "PC")
		       (and  (= prefix "BP")(> (atof ref) 1699))
		       (= prefix "CD")
		       (and (= prefix "LP")(> (atof ref) 145855))
		       (and (= prefix "CP")(> (atof ref) 157285))
		       (and (= prefix "CS")(> (atof ref) 1386))
		       (and (= prefix "AP")(> (atof ref) 59999))
		       (and (= prefix "SP")(> (atof ref) 20337))


		       
		       )
		 (progn
	       
	        (setq rn (/(+
				  (* (- (ascii (substr prefix 1 1)) 64) 9)
				  (* (- (ascii (substr prefix 2 1)) 64) 8)
				  (* (atof (substr ref 1 1)) 7)
				  (* (atof (substr ref 2 1)) 6)
				  (* (atof (substr ref 3 1)) 5)
				  (* (atof (substr ref 4 1)) 4)
				  (* (atof (substr ref 5 1)) 3)
				  (* (atof (substr ref 6 1)) 2)
				  )
				  23))
		      (setq rn (+ (* (- rn (fix rn)) 23) 1))
	       (setq rn (atof (rtos rn 2 0)))
		       (setq cchkdigit (substr cdalf (fix rn) 1))


	       
	       (if (= subpos (+ (strlen textstring) 1));if no check digit at end of textstring
		 (progn;is at end of textstring
		   (princ (strcat "\n" prefix Ref ))
		   )
		 (progn;if normal check digit

		   (setq chkdigit (substr textstring subpos 1))
		   
		   (if (or (< (ascii chkdigit) 63)
			   (> (ascii chkdigit) 90))
		     (progn;if not a letter of the alphabet
		       		   (princ (strcat "\n" prefix Ref cchkdigit))
		   		       )
		     (progn;if it is a letter of the alphbet
		      
		       (if (/= chkdigit cchkdigit)(princ (strcat "\n" prefix ref chkdigit " is incorrect - check digit calulates as " cchkdigit)))
			      
	       ));if chkdigit is letter of alphabet
	     

		     
	       (setq filename (strcat prefix ref cchkdigit ))

		 ));if check digit is at end of text string
		      ));is a number to check

	       (setq filename (strcat prefix ref cchkdigit ))

	       ));if 3rdnum is a number


(setq FF nil);empty findfile


  
 (setq count 0)
  ;just look for the plan as entered (do this first)
  (repeat (length autoloadlist)
  (IF (= FF NIL )
    (progn
      (SETQ FF (FINDFILE (strcat autoloadff (nth count autoloadlist) "\\"textstring ".xml")))
    (if (/= FF NIL)(SETQ afrpath (nth count autoloadlist)))
      ));if not found
    
  (setq count (+ count 1))
  );r

(setq count 0)
  ;look for plan with leading 00 and checkdigitv (do this second)
(repeat (length autoloadlist)
  (IF (= FF NIL )
    (progn
      (SETQ FF (FINDFILE (strcat autoloadff (nth count autoloadlist) "\\" filename ".xml")))
      (if (/= FF NIL)(SETQ afrpath (nth count autoloadlist)))
      ));if not nil
  (setq count (+ count 1))
  );r


  (if (/= ff nil)
    (progn
  (setq afrlist (list))
  (setq count 0)
  ;check for AFR files (do this third)

  (setq afrcount 1)
  (repeat 100
  (IF  (FINDFILE (strcat autoloadff afrpath "\\" filename "_AFR" (rtos afrcount 2 0)".xml"))
    (setq afrlist (append afrlist (list(strcat  filename "_AFR" (rtos afrcount 2 0)".xml"))))
    )
    (setq afrcount (+ afrcount 1))
  );end repeat 100 looks for afr

  ));if ff not nil
    

  (if (> (length afrlist) 0)
        (progn
      (setq afralert "")
      (setq count 0)
      (repeat (length afrlist)
	(setq afralert (strcat afralert "\n" (nth count afrlist)))
	(setq count (+ count 1))
	);r
    (alert (strcat "Plan has AFR files" afralert))
      ));p&if afr plans


  

 

(if (= ff nil)(exit))


  )

  

       


;----------------------------------------------------------------IMPORT XML--------------------------------------------

(DEFUN C:XIN (/)
(setq importcgpoint "fromfile")
  (setq startpointreq "N")
  (setq simplestop "0")
  (setq filescript "0")
  (xmlimporter)
    )

(defun C:XINO (/)
(SETQ importcgpoint "fromobs")
  (setq startpointreq "Y")
  (setq simplestop "0")
  (setq filescript "0")
  (xmlimporter)
  )

(defun C:XINOA (/)
  (princ (strcat "\n Looking for xmls in folder   " autoloadff))
(SETQ importcgpoint "fromobs")
  (setq startpointreq "Y")
  (setq simplestop "0")
  (setq filescript "2")
  (findmyfile)
  (xmlimporter)

  (if (> (length afrlist) 0)
    (progn
      (setq startpointreq "N")
      (setq afrcount 0)
  (repeat (length afrlist)
    (setq ff (strcat autoloadff afrpath "\\" (nth afrcount afrlist)))
    (setq startpoint afrstartpoint)
    (xmlimporter)
    (setq afrcount (+ afrcount 1))
    );r
      ));p&if afr
  );defun

(defun C:XINS (/)
(SETQ importcgpoint "fromobs")
  (SETQ simplestop "1")
  (xmlimporter)
  )

(defun XMLIMPORTER (/)
   (setq prevlayer (getvar "CLAYER"))
  (SETVAR "CLAYER"  "Lot Definitions" )
(setq cgpointlist (list));list of coordinate geometery points
  (setq pmlist (list));list of pms from the monument and cg points list
  (setq drawpmlist (list));list of drawn pms so the same pm is not drawn twice
  (setq rmlist (list));list of monuments from monument lis
  (setq occlist (list));list of occupation points
  (setq drawnrmlist (list));list of drawn mounments,so the same rm is not drawn twice
  (setq drawndref (list));list of drawn double references,so same dref is not drawn twice
  (setq reflist (list));list of drawn references, to allow the same rm to be referenced twice
  (setq easementlist (list));list of lines which are easement lines
  (setq extglist (list));list of lines which are the lot being extingushed
  (setq otherlines (list));list of lot and road lines from lots to remove from easementlist
  (setq finaleaselist (list));list of reduced easement lines
  (setq linelist (list));list of lines which have been drawn
  (setq arclist (list));list of arcs which have been drawn
  (setq poplist (list));list of pops for creation at the end
  (setq islist (list));list of instrument stations for cg point reference
  (setq pmboxmark nil);reset spot for coordinte box
  (setq irlinetestlist nil);list of plotted irlines
  (setq rmornot "");tool for identifying RM gone
  

 
 

  (if (= filescript "1") (setq xmlfilen (getstring "File:" t)))
  (if (= filescript "0") (setq xmlfilen (getfiled "Select XML file" "" "xml" 2)))
  (if (= filescript "2") (setq xmlfilen ff))
  (if (= startpointreq "Y")(progn
			     (setq startpoint (getpoint "Select a starting point:"))
			     (setq afrstartpoint startpoint)
			     ))
  

   (setq xmlfile (open xmlfilen "r"))
  
  (setq linetext "")
  (setq maxeast -100000000000000000000000000000.1)
  (setq minnorth 10000000000000000000000000000.1)
  (setq maxnorth -100000000000000000000000000000.1)
  (setq daf1 "")
  (setq nlf "0")


  ;VIC
  (setq exttlist (list));extigusihed titles list
  (setq extplist (list));extigusihed plans list
  (setq extalist (list));extigusihed address list
  (setq dllist (list));depth limitations
  (setq rbenlist (list));restrictio benifit list
  (setq rburlist (list));restriction burden list
  (setq resnamelist (list));restriction name list
  (setq trilist (list));list of triangle stns placed
  (setq pmcolist (list));list of PM coodinates so their monuments arent made
  (setq builddrawn (list));list of strcutural wall that have been drawn
(setq surheadno nil)


  
  ;CHECK TO MAKE SURE FILE IS LINEFED
  (setq linetext (read-line xmlfile))
 
  
  ;Get plan number
  (princ "\nSearching for Survey header")
 (while (and (/= linetext nil) (= (setq surheadno (vl-string-search "<SurveyHeader" linetext)) nil))( progn
   (linereader)
      ))

  (if (> surheadno 50)
    (progn

      (setq outfilen (vl-string-subst "_WLF." "." xmlfilen))
  (setq outfile (open outfilen "w"))

       (princ (strcat "\nNon linefed file detected, to allow importing created file at " outfilen ))
(close xmlfile)
(setq xmlfile (open xmlfilen "r"))

      (setq linetext (read-line xmlfile))    
      (while (/= nil linetext)
       (progn

;add linefeed a every <
	 (if (/= linetext "")
	   (progn
						 (setq <pos 1)
	      (while (/=  (setq <pos (vl-string-search "<" linetext <pos )) nil) (setq linetext (vl-string-subst (strcat (chr 10) "<") "<"   linetext <pos)
										      <pos (+ <pos 2)))
							(write-line linetext outfile)
	   ))
      (setq linetext (read-line xmlfile))
	 
	 ))
      
      (close outfile)
      (close xmlfile)
      (setq xmlfilen outfilen);set reading file to WLF file
        (setq xmlfile (open xmlfilen "r"))
      (linereader)

       (while (= (vl-string-search "<SurveyHeader" linetext) nil)( progn
   (linereader)
      ))

      
      ))
      
  
 
  (if (/= (setq stringpos (vl-string-search "name" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
(setq planname (substr linetext (+ stringpos 7) (-(- wwpos 2)(+ stringpos 5))))
))
  (close xmlfile)
  (setq xmlfile (open xmlfilen "r"))

 
					     
(princ "\nReading Header")

  
  ;XML HEADER
    ;linefeed to cgpoints
  (while (= (vl-string-search "<CoordinateSystem" linetext) nil)( progn
   (linereader)
))
  (if (/= (setq stringpos (vl-string-search "datum" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 7)))(setq datum (substr linetext (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq datum ""))
   (if (/= (setq stringpos (vl-string-search "desc" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq datum (strcat datum "~" (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))))
    (if (/= (setq stringpos (vl-string-search "horizontalDatum" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 17)))(setq hdatum (substr linetext (+ stringpos 18) (-(- wwpos 1)(+ stringpos 16)))))(setq hdatum ""))
  (if (/= (setq stringpos (vl-string-search "verticalDatum" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))(setq vdatum (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq vdatum ""))
  
  (princ "\nReading Monuments")
  

  ;MONUMENTS--------------------------------------
  ;linefeed to monuments, as they control linetypes and pm and other notation
  
 (while (and (= (vl-string-search "</LandXML>" linetext ) nil) (= (vl-string-search "<Monuments>" linetext ) nil)) ( progn
 (linereader)
 
))

  (if (vl-string-search "<Monuments>" linetext )(progn ;if not end of file
						  
    (linereader)
     (while (= (vl-string-search "</Monuments>" linetext ) nil) ( progn
   (if (/= linetext "" )
     (progn
       (if (/= (setq stringpos (vl-string-search "name" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq monname (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq monname nil))
       (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))(setq moncgp (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7)))))(setq moncgp nil))
       (if (/= (setq stringpos (vl-string-search "type" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq montype (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq montype nil))
       (if (/= (setq stringpos (vl-string-search "desc" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq mondesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq mondesc nil))
       (if (/= (setq stringpos (vl-string-search "state" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 7)))(setq monstate (substr linetext (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq monstate nil))
       (if (/= (setq stringpos (vl-string-search "originSurvey" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 14)))(setq monrefdp (substr linetext (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq monrefdp nil))
       (if (/= (setq stringpos (vl-string-search "condition" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 11)))(setq moncond (substr linetext (+ stringpos 12) (-(- wwpos 1)(+ stringpos 9)))))(setq moncond nil))

       ;deal with a state of gone
       
       ;add pms and ssms to pmlist
       (if (or (= montype "PM")(= montype "SSM"))
	 (setq pmlist (append pmlist (list moncgp)(list linetext)))
	 )
       ;check if pm is also a referece mark
       ;(if (and (or (= montype "PM")(= montype "SSM")) (= desc "used as reference mark"))
;	 (setq rmlist (append rmlist (list moncgp)(list linetext)))
;	 )
       ;add everything other than a pm or ssm to rmlist
       (if (/= montype "Occupation")
	 (setq rmlist (append rmlist (list moncgp)(list linetext)))
	 (setq occlist (append occlist (list moncgp)(list linetext)));else - is an occ
	 )
       
       ));if not linefeed

   (linereader)
   ));while not </Monuments>

));if not end of file monument storer

  (princ "\nReading Instrument Stations")
  ;create intrument station list-----------------------------------------------------------
  (close xmlfile)
  (setq xmlfile (open xmlfilen "r"))

  ;linefeed to end of survey header
(linereader)

  (while(= (vl-string-search "</SurveyHeader>" linetext ) nil) ( progn
 (linereader)
))
 
  
  ;instrument stations
 (linereader)
(while (= (vl-string-search "</Survey>" linetext ) nil)( progn
								 (if (vl-string-search "<InstrumentSetup" linetext )(progn
		   (if (/= (setq stringpos (vl-string-search "id" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 4)))(setq isid (substr linetext (+ stringpos 5) (-(- wwpos 1)(+ stringpos 3)))))(setq isid nil))
		   (linereader)
		   (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))(setq iscgp (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7)))))(setq iscgp nil))
		   (setq isid (strcat "IS-" isid));add "is-" to isid just in case someone is stupid enough to use the same id
		   (setq islist (append islist (list isid)(list iscgp)))

		   (linereader);read end of is
		   ))
		   (linereader);read next line
		   
));p&w not observation group



  
(close xmlfile)
   (setq xmlfile (open xmlfilen "r"))


  (if (= importcgpoint "fromfile")
    (progn

      (princ "\nReading CG points")
  
  ;CGPOINTS-------------------------------------
  ;linefeed to cgpoints
 (while (= (vl-string-search "<CgPoints" linetext) nil) ( progn
 (linereader)
))


  ;get zone if present
  (if (/= (setq stringpos (vl-string-search "zoneNumber" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 12)))(setq zone (substr linetext (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11)))))(setq zone ""))
  
 (linereader)
  ;do until end of cgpoints

 (while (= (vl-string-search "</CgPoints>" linetext) nil) ( progn
     
    (if (/= (vl-string-search "<CgPoint" linetext )nil)
     (progn
       ;store line information
       (if (/= (setq stringpos (vl-string-search "state" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 7)))(setq cgpstate (substr linetext (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq cgpstate nil))
              (if (/= (setq stringpos (vl-string-search "pntSurv" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))(setq cgpntsurv (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8)))))(setq cgpntsurv nil))
              (if (/= (setq stringpos (vl-string-search "name" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq cgpname (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq cgpname nil))
                     (if (/= (setq stringpos (vl-string-search "oID" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 5)))(setq cgpoID (substr linetext (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4)))))(setq cgpoID nil))
                            (if (/= (setq stringpos (vl-string-search "desc" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq cgdesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq cgdesc nil))
       


       ;VIC - REMOVED, USED TO ID SMM OR PM NOt NECESSARY
       ;check if point is a pm and store in drawpmlist
       ;(if (/= cgpoID nil)(progn
	;		    (setq remlist (member cgpname pmlist))
	;		    (setq pmline (cadr remlist))
	;		    (if (/= (setq stringpos (vl-string-search "type" pmline)) nil)(progn
	;		    (setq wwpos (vl-string-position 34 pmline (+ stringpos 6)))(setq pmstart (substr pmline (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))
	;		      (setq pmstart ""))
	;		       (if (/= (setq stringpos (vl-string-search "state" pmline)) nil)(progn
	;		    (setq wwpos (vl-string-position 34 pmline (+ stringpos 7)))(setq pmstate (substr pmline (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))
	;		      (setq pmstate ""))
	;			 (setq drawpmlist (APPEND drawpmlist (list cgpname)(list (strcat pmstart cgpoID ))(list pmstate )))
	;		    )
	 ;)
              	  
			    
        ;get cgpoint coordintes
                            (if (/= (setq stringpos (vl-string-search ">" linetext )) nil)(progn
(setq <pos (vl-string-position 60 linetext (+ stringpos 2)))
(if (= <pos nil)(setq <pos 2000))
   
(setq spcpos (vl-string-position 32 linetext stringpos))
(setq north (atof (substr linetext (+ stringpos 2) (- spcpos (+ stringpos 1) ))))
(setq east (substr linetext (+ spcpos 2) (- (- <pos 1) spcpos )))
(if (/= (setq spcpos2 (vl-string-position 32 east 1)) nil)(progn
							    (setq height (substr east (+ spcpos2 2) 200))
							    (setq east (substr east 1 (+ spcpos2 2)));if height exists
								)
  (setq height "0"));else
(setq east (atof east))
;if in lats and longs convert to easting northing
(if (vl-string-search "latitude=" linetext )(progn
					      (setq p1 (list east north))
					      (LL2MGA)
					      ))

(if (> east maxeast) (setq maxeast east))
(if (< north minnorth)(setq minnorth north))
(if (> north maxnorth)(setq maxnorth north))
(setq east (rtos east 2 6))
(setq north (rtos north 2 6))

(setq cgco (strcat east "," north))
)(setq cgco nil))
       (setq cgpointlist (append cgpointlist (list cgpname) (list cgco)(list (substr cgpntsurv 1 1))))

       ;if datum point draw datum point and label
(if (/= cgdesc nil)(progn
		   	 (SETVAR "CLAYER"  "Datum Points" )
		      (COMMAND "POINT" (strcat east "," north "," height))
		 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 cgdesc)))))
		     (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
       (SETQ TEXTPOS (LIST (- (atof east) TH) (+ (atof north) (* 0.5 TH))))
		 
  		 (if (= spcpos2 nil)(progn
		       (SETVAR "CLAYER"  "Drafting AFR" )
  		 (COMMAND "TEXT" "J" "BR"  TEXTPOS (* TH 2) "90" (STRCAT "'" cgdesc "'"));normal datum point

			 )
		       (progn;else stratum datum point
			 (SETVAR "CLAYER"  "Drafting" )
			 (COMMAND "TEXT" "J" "BR"  TEXTPOS (* TH 1.4) "90" (STRCAT "'" cgdesc "'"))
			 (COMMAND "TEXT" "J" "BL"  (LIST (ATOF EAST)(ATOF NORTH)(ATOF HEIGHT)) (* TH 1) "45" (rtos (atof height) 2 3))
			 );P
		       );IF SPCPOS2 NIL
  ));p&if datum

       

       ;DRAW CG POINTS
       (SETVAR "CLAYER"  "CG Points" )
       (setq p1 (list (atof east) (atof north)))
       (command "point" p1)
       (COMMAND "TEXT" "J" "BL"  P1 (* TH 0.25) "90" (strcat cgpname (substr cgpntsurv 1 1)(substr cgpstate 1 1)))
       ));p and if <cgpoint
     
(linereader)
  
   );p
   );while

 ))

  (if (= importcgpoint "fromobs")
    (obsimporter)
    )

  (if ( = simplestop "1") (exit))

  ;create additional startpoint for afr
  (setq afrstartpoint (list (car afrstartpoint) (+ (cadr afrstartpoint) (* 1.5 (- maxnorth minnorth)))))
  

  ;PARCELS-------------------------------------

  (princ "\nReading Parcels")

  (close xmlfile)
   (setq xmlfile (open xmlfilen "r"))
  
  (setq elp (list (+ maxeast 24) (+ minnorth 108)));setup postion for easement legend
  (setq ocp (list (+ maxeast 162) (+ minnorth 260)));setup postion for owners corporation entitlements
  (setq rlp (list (+ maxeast 308) (- minnorth 35)));setup postion for restrictions
  (setq rdlp (list (+ maxeast 38) (+ minnorth 255)));setup postion for road table
  (setq srp (list (+ maxeast 460) (+ minnorth 420)));setup postion for survey report

  
    ;linefeed to parcels
 (while (= (vl-string-search "<Parcels>" linetext ) nil) ( progn
  (linereader)
))


  ;do until end of parcels
       (while (= (vl-string-search "</Parcels>" linetext ) nil) ( progn

;linefeed to next parcel
  (while (= (vl-string-search "<Parcel " linetext ) nil) ( progn
  (linereader)
))
  ;(princ (strcat "\nParcelline :" linetext))


  
(SETQ ENTSS (SSADD))
 (SETQ EASESS (SSADD))
  (setq titles "")
  (setq smalladdress "")
  (setq irplotlist (list))
  
					  
;get parcel info
       (if (/= (setq stringpos (vl-string-search "name=" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq pclname (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq pclname ""))
       (if (/= (setq stringpos (vl-string-search "class=" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 7)))(setq pclclass (substr linetext (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq pclclass ""))
       (if (/= (setq stringpos (vl-string-search "state=" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 7)))(setq pclstate (substr linetext (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq pclstate ""))
       (if (/= (setq stringpos (vl-string-search "parcelType=" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 12)))(setq pcltype (substr linetext (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11)))))(setq pcltype ""))
       (if (/= (setq stringpos (vl-string-search "parcelFormat=" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 14)))(setq pclformat (substr linetext (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq pclformat ""))
       (if (/= (setq stringpos (vl-string-search "area=" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq pclarea (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq pclarea ""))
       (if (/= (setq stringpos (vl-string-search "desc=" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq pcldesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq pcldesc ""))
       (if (/= (setq stringpos (vl-string-search "useOfParcel=" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 13)))(setq pcluse (substr linetext (+ stringpos 14) (-(- wwpos 1)(+ stringpos 12)))))(setq pcluse ""))
  (if (/= (setq stringpos (vl-string-search "owner=" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 7)))(setq pclowner (substr linetext (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq pclowner ""))
  (if (/= (setq stringpos (vl-string-search "pclRef=" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))(setq pclref (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7)))))(setq pclref ""))

					    (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" pcluse &pos )) nil) (setq pcluse (vl-string-subst "&" "&amp;"  pcluse &pos)
										      &pos (+ &pos 4)))
   (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" pcldesc &pos )) nil) (setq pcldesc (vl-string-subst "&" "&amp;"  pcldesc &pos)
										      &pos (+ &pos 4)))

     (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" pclowner &pos )) nil) (setq pclowner (vl-string-subst "&" "&amp;"  pclowner &pos)
										      &pos (+ &pos 4)))

					    ;deal with muilti parts, easements, depth limitations and owners corpoations links
(if (or (and (= pclclass "Easement")(/= pclformat "Geometry"))(= pclclass "Depth Limitation")(and (= pclclass "Restriction")(/= pcldesc "")(= pcltype "Multipart"))(= pclclass "Owners Corporation")(= pcltype "Multipart"))
					      (progn

						;multipart  
						    (if (and (= pcltype "Multipart")(= pcluse ""))(progn
												    ;MEROD(princ (strcat "\nmulti:" linetext))
							      (while(= (vl-string-search "</Parcel>" linetext ) nil)( progn
														      
														      (linereader)
														      
														      ));p&w
												     
												    ;MEROD(princ (strcat "\nmulti  end:" linetext))
							      ))

						;Restriction
						(if (= pclclass "Restriction")(progn
										;MEROD(princ (strcat "\nrestriction:" linetext))
										
										(setvar "CLAYER" "lot definitions")
										(command "point" rlp)
						(if (/= pcldesc "")(setq pcldescs (strcat " desc=\"" pcldesc "\""))(setq pcldescs ""))
						  (if (/= pclarea "")(setq areas (strcat " area=\"" pclarea "\""))(setq areas ""))
						  (if (/= pcluse "")(setq pcluses (strcat " useOfParcel=\"" pcluse "\""))(setq pcluses ""))
						  (if (/= pclowner "")(setq pclowners (strcat " owner=\"" pclowner "\""))(setq pclowners ""))
						  (if (/= pclformat "")(setq pclformats (strcat " parcelFormat=\"" pclformat "\""))(setq pclformats ""))
										 (if (= pclstate "created")(progn
													     (setq slashpos1 (vl-string-position 92 pclname 0))
													     (setq pclname (substr pclname 1 slashpos1))
													     ))
				   
						  (SETQ LTINFO (STRCAT "  <Parcel name=\"" pclname "\" class=\"" pclclass "\" state=\"" pclstate "\" parcelType=\"" pcltype "\""  pclformats areas pcluses pclowners ">"))
						    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)
										(setq resnamelist (append resnamelist (list pclname pcldesc)))
										;add restriction name to list for processing at end of parcels


										
										;(princ (strcat "\nrestriction  end:" linetext))
										
										));if restriction
												      
										
				
							
						  
						
						;Easement Links
						(if (and (= pclclass "Easement")(/= pclformat "Geometry"))
						    (progn
						      ;MEROD(princ (strcat "\neasement def:" linetext))
						      						    
						    (linereader);parcels
						    (linereader);link parcel
						    (setq pclref "")
						    (while (/= (setq stringpos (vl-string-search "pclRef" linetext )) nil)(progn
                                                    (setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
						    (setq pclref (strcat pclref "," (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7)))))
						    (linereader);next link of /parcels
						    ))
						    (setq pclref (substr pclref 2))
						    
						    (setvar "CLAYER" "lot definitions")
						    (command "point" elp)
						    
						  (if (/= pcldesc "")(setq pcldescs (strcat " desc=\"" pcldesc "\""))(setq pcldescs ""))
						  (if (/= pclarea "")(setq areas (strcat " area=\"" pclarea "\""))(setq areas ""))
						  (if (/= pcluse "")(setq pcluses (strcat " useOfParcel=\"" pcluse "\""))(setq pcluses ""))
						  (if (/= pclowner "")(setq pclowners (strcat " owner=\"" pclowner "\""))(setq pclowners ""))
						    (if (/= pclformat "")(setq pclformats (strcat " parcelFormat=\"" pclformat "\""))(setq pclformats ""))
						     (if (= pclstate "created")(progn
										 ;(setq slashpos1 (vl-string-position 92 pclname 0))
										 ;(setq pclname (substr pclname 1 slashpos1))
										 (setq origin "THIS PLAN")
													     )
						       (SETQ origin pclname)
						       )
						  (SETQ LTINFO (STRCAT "  <Parcel name=\"" pclname "\"" pcldescs  " class=\"" pclclass "\" state=\"" pclstate "\" parcelType=\"" pcltype "\""  pclformats  areas pcluses pclowners ">!" pclref))
						    (if (> (strlen ltinfo) 255) (progn
										  (princ (strcat "\nEasement " pclname " purpose too long for xdata 255 character limit, truncating"))
										  (setq pcluses (strcat " useOfParcel=\"" (substr pcluse 1 (- (strlen pcluse) (- (strlen ltinfo) 255)))"\""))
							(SETQ LTINFO (STRCAT "  <Parcel name=\"" pclname "\"" pcldescs  " class=\"" pclclass "\" state=\"" pclstate "\" parcelType=\"" pcltype "\""  pclformats  areas pcluses pclowners ">!" pclref))
										  ))
			  

						    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (PRINC (STRCAT "\n" (RTOS (STRLEN LTINFO) 2 0)))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO))))) 
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)

						    (setvar "CLAYER" "Drafting")

						    (COMMAND "text" elp "2.5" "90" pclref )
						    (COMMAND "TEXT" (LIST (+ (CAR ELP) 27)(CADR ELP)) "2.5" "90" pcluse )
						    (COMMAND "TEXT" (LIST (+ (CAR ELP) 120)(CADR ELP)) "2.5" "90" origin )
						    (COMMAND "TEXT" (LIST (+ (CAR ELP) 160)(CADR ELP)) "2.5" "90" pclowner )

						    (setq elp (list (car elp)(- (cadr elp) 4)))

						    (while (= (vl-string-search "</Parcel>" linetext) nil)(linereader));read to end of parcel in case some goose put an address on a easement PS820112R
						    ; (linereader);/parcel

						      ;MEROD(princ (strcat "\neasement def end:" linetext))
						    						    
						    ));p&if easement link


						;Owners Corporation Links
						(if (= pclclass "Owners Corporation")
						  (progn
						    ;MEROD(princ (strcat "\nOC:" linetext))
						    (setvar "CLAYER" "lot definitions")
						    (command "point" ocp)
						    (if (/= pcldesc "")(setq pcldescs (strcat " desc=\"" pcldesc "\""))(setq pcldescs ""))
						    (if (/= pcluse "")(setq pcluses (strcat " useOfParcel=\"" pcluse "\""))(setq pcluses ""))
						  

						    (SETQ LTINFO (STRCAT "  <Parcel name=\"" pclname "\"" pcldescs  " class=\"" pclclass "\" state=\"" pclstate "\" parcelType=\"" pcltype "\"" pcluses  ">"))
						    (SETQ SENT (ENTLAST))
						    (SETQ OCENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)


						    
						    (setvar "CLAYER" "Drafting")
						    (COMMAND "text" ocp "2.5" "90" "PARCEL" )
						    (COMMAND "TEXT" (LIST (+ (CAR ocp) 40)(CADR ocp)) "2.5" "90" "ENTITLEMENT" )
						    (COMMAND "TEXT" (LIST (+ (CAR ocp) 70)(CADR ocp)) "2.5" "90" "LIABILITY" )
						    
						    (setq ocp (list (car ocp)(- (cadr ocp) 4))) 

						   
						    (linereader);read first line under oc
						    ;(linereader);link parcel
						    (while(= (vl-string-search "</Parcel>" linetext ) nil)
						      (progn
							
							(if (and (/= (vl-string-search "<Parcel" linetext) nil)(= (vl-string-search "<Parcels>" linetext) nil));only read parcel strings
								 (progn
													
													     
						    (if (/= (setq stringpos (vl-string-search "pclRef" linetext )) nil)(progn
							(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))(setq pclref (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7)))))(setq pclref ""))
						    (if (/= (setq stringpos (vl-string-search "lotEntitlements" linetext )) nil)(progn
							(setq wwpos (vl-string-position 34 linetext (+ stringpos 17)))(setq lotent (substr linetext (+ stringpos 18) (-(- wwpos 1)(+ stringpos 16)))))(setq lotent ""))
						    (if (/= (setq stringpos (vl-string-search "liabilityApportionment" linetext )) nil)(progn
							(setq wwpos (vl-string-position 34 linetext (+ stringpos 24)))(setq lotliab (substr linetext (+ stringpos 25) (-(- wwpos 1)(+ stringpos 23)))))(setq lotliab ""))
						    (setvar "CLAYER" "lot definitions")
						    (command "point" ocp)
						    
						    (SETQ LTINFO (STRCAT "  pclRef=\"" pclref "\" lotEntitlements=\"" lotent "\" liabilityApportionment=\"" lotliab "\"/>"))
						    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)

						    (setvar "CLAYER" "Drafting")

						    (COMMAND "text" ocp "2.5" "90" pclref )
						    (COMMAND "TEXT" (LIST (+ (CAR ocp) 50)(CADR ocp)) "2.5" "90" lotent )
						    (COMMAND "TEXT" (LIST (+ (CAR ocp) 75)(CADR ocp)) "2.5" "90" lotliab )

						    (setq ocp (list (car ocp)(- (cadr ocp) 4)))

						    ;read closing line or next line if not closed parcel

						    ));p&if link parcel string
						    
				    
						    
(if (/= (vl-string-search "<LocationAddress" linetext ) nil)(progn;check for address string
							      ;MEROD(princ (strcat "\noc address:" linetext))
							       
							   (if (/= (setq stringpos (vl-string-search "addressType" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 13)))(setq addresstype (substr linetext (+ stringpos 14) (-(- wwpos 1)(+ stringpos 12)))))(setq addresstype ""))
							   (if (/= (setq stringpos (vl-string-search "numberFirst" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 13)))(setq numberfirst (substr linetext (+ stringpos 14) (-(- wwpos 1)(+ stringpos 12)))))(setq numberfirst ""))
							   (if (/= (setq stringpos (vl-string-search "numberSuffixFirst" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 19)))(setq numberSuffixFirst (substr linetext (+ stringpos 20) (-(- wwpos 1)(+ stringpos 18)))))(setq numberSuffixFirst ""))
							     (if (/= (setq stringpos (vl-string-search "numberLast" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 12)))(setq numberLast (substr linetext (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11)))))(setq numberLast ""))
							   (if (/= (setq stringpos (vl-string-search "numberSuffixLast" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 18)))(setq numberSuffixLast (substr linetext (+ stringpos 19) (-(- wwpos 1)(+ stringpos 17)))))(setq numberSuffixLast ""))
							   (if (/= (setq stringpos (vl-string-search "flatNumber" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 12)))(setq flatNumber (substr linetext (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11)))))(setq flatNumber ""))
							   (if (/= (setq stringpos (vl-string-search "flatType" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 10)))(setq flatType (substr linetext (+ stringpos 11) (-(- wwpos 1)(+ stringpos 9)))))(setq flatType ""))
							   (if (/= (setq stringpos (vl-string-search "floorLevelNumber" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 18)))(setq floorLevelNumber (substr linetext (+ stringpos 19) (-(- wwpos 1)(+ stringpos 17)))))(setq floorLevelNumber ""))
							   (if (/= (setq stringpos (vl-string-search "floorLevelType" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 16)))(setq floorLevelType (substr linetext (+ stringpos 17) (-(- wwpos 1)(+ stringpos 15)))))(setq floorLevelType ""))

							   
							   (linereader);read road line


							   (if (/= (setq stringpos (vl-string-search "roadName=" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 10)))(setq roadName (substr linetext (+ stringpos 11) (-(- wwpos 1)(+ stringpos 9)))))(setq roadName ""))
							    (if (/= (setq stringpos (vl-string-search "roadNameSuffix" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 10)))(setq roadNameSuffix (substr linetext (+ stringpos 11) (-(- wwpos 1)(+ stringpos 9)))))(setq roadNameSuffix ""))
							   (if (/= (setq stringpos (vl-string-search "roadNameType" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 14)))(setq roadNameType (substr linetext (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq roadNameType ""))
							   (if (/= (setq stringpos (vl-string-search "roadType" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 10)))(setq roadType (substr linetext (+ stringpos 11) (-(- wwpos 1)(+ stringpos 9)))))(setq roadType ""))

							   (linereader);read nextline

							   (if (/= (setq stringpos (vl-string-search "adminAreaName" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))(setq adminAreaName (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq adminAreaName ""))
							   (if (/= (setq stringpos (vl-string-search "adminAreaType" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))(setq adminAreaType (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq adminAreaType ""))
							   (if (/= (setq stringpos (vl-string-search "adminAreaCode" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))(setq adminAreaCode (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq adminAreaCode ""))

							   (setq comboaddress (strcat numberfirst numbersuffixfirst " " roadname " " roadnametype " " adminareaname " " adminareacode))
							   (if (and (= (member comboaddress extalist) nil) (= pclstate "extinguished"))(setq extalist (append extalist (list comboaddress))))
							   (setq smalladdress (strcat "<smalladdress=\"" flatNumber ","
										     flatType ","
										     floorLevelNumber ","
										     floorLevelType ","
										     numberfirst ","
										     numberSuffixFirst ","
										     numberLast ","
										     numberSuffixLast ","
										     roadName ","
										     roadnamesuffix ","
										     roadnametype ","
										     roadtype ","
										     adminareaname ","
										     adminAreaCode "\"/>"))
							   ;read nextline (</LocationAddress>
						;	   (linereader);read nextline 



    	    (SETQ XDATAI (ENTGET OCENT '("LANDXML")))
	    (SETQ XDATAI (ASSOC -3 XDATAI))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))

   
  (setq stringpossearch 1)
  (while (/= nil (setq stringpossearch (vl-string-search ">" xdatai (+ stringpossearch 1))))
    (setq stringpos stringpossearch)
    )
    
  (setq stringpos1 (+ stringpos 1)
	stringpos2 (+ stringpos 2) 
	)
   
(setq xdatafront (substr xdatai 1  stringpos1 ))
    (setq xdataback (substr xdatai stringpos2 ))
    (setq xdatai (strcat xdatafront smalladdress xdataback))

    (SETQ SENTLIST (ENTGET OCENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 xdatai)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
							      ;MEROD(princ (strcat "\noc address end:" linetext))

    ));if location

	(linereader);read nex line in oc					    
));p&while not /parcels
						    
						   
						      
;MEROD(princ (strcat "\noc end:" linetext))
						 
						    						    
						    ));p&if oc link

						;Depth limitation

					            (if (= pclclass "Depth Limitation")(progn
											 
											 ;(princ (strcat "\ndepth lim:" linetext))
											 (setq dlname (substr pclname 3 ))
										     (setq dllist (list  pcldesc))
											 (if (= (vl-string-search "/>" linetext) nil)
											   (progn
											     (while (= (vl-string-search "</Parcel>" linetext) nil)(linereader))
											       
											     ));if not a single line depth limitation
											   
										     ))
						  
						  
;(linereader);get next parcel info

	;---------------------------END OF MEROD(princ (strcat "\nMEROD end:" linetext))-----------------------------------------------------------
							      							  )
					      (progn ;else continue on - not multipart etc


						;single parcel restriction
						(if (and (= pclclass "Restriction")(/= pcltype "Part"))(progn
										;MEROD(princ (strcat "\nrestriction:" linetext))
										
										(setvar "CLAYER" "lot definitions")
										(command "point" rlp)
						(if (/= pcldesc "")(setq pcldescs (strcat " desc=\"" pcldesc "\""))(setq pcldescs ""))
						  (if (/= pclarea "")(setq areas (strcat " area=\"" pclarea "\""))(setq areas ""))
						  (if (/= pcluse "")(setq pcluses (strcat " useOfParcel=\"" pcluse "\""))(setq pcluses ""))
						  (if (/= pclowner "")(setq pclowners (strcat " owner=\"" pclowner "\""))(setq pclowners ""))
						  (if (/= pclformat "")(setq pclformats (strcat " parcelFormat=\"" pclformat "\""))(setq pclformats ""))
										 (if (= pclstate "created")(progn
													     (setq slashpos1 (vl-string-position 92 pclname 0))
													     (setq pclname (substr pclname 1 slashpos1))
													     ))
				   
						  (SETQ LTINFO (STRCAT "  <Parcel name=\"" pclname "\" class=\"" pclclass "\" state=\"" pclstate "\" parcelType=\"" pcltype "\""  pclformats areas pcluses pclowners ">"))
						    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)
										(setq resnamelist (append resnamelist (list pclname pcldesc)))
										;add restriction name to list for processing at end of parcels


										
										;MEROD(princ (strcat "\nrestriction  end:" linetext))
										
										));if restriction

						

(if (= (vl-string-search "/>" linetext) nil)(progn;is not a single line restriction
  


						

						;REMOVE PLAN NAME FOR CREATED PARCELS
						
				 (if (= pclstate "created")(progn
		 (setq slashpos1 (vl-string-position 92 pclname 0))
		(setq pclname (substr pclname 1 slashpos1))
		))


				  (setq irtextlist nil);clear irregular lists
			          (setq irplotlist nil)
				 
					      
       (linereader);read centrepoint line
					    
					      
														      
															 

        (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
(setq lotc (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
(setq remainlist (member lotc cgpointlist))
(setq lotc (cadr remainlist))
(setq ,pos1 (vl-string-position 44 lotc 0))
(setq east (atof (substr lotc 1 ,pos1)))
(setq north (atof (substr lotc (+ ,pos1 2) 50)))
(setq lotc (list east north))
)(setq pclcenpos nil))

					    (IF (= pclstate "extinguished")(setvar "clayer" "Drafting AFR") (setvar "clayer" "Drafting"))
       (setvar "dimzin" 0)
					    
					    ;make area to 4 significant figures
					    (if (/= "" pclarea) (progn
					    (setq area (atof pclarea))
					    (if (> area 0)(setq pclareas (strcat (rtos (*  (/ area 0.1) 0.1) 2 1) "m²")))
					    (if (> area 100)(setq pclareas (strcat (rtos (*  (/ area 1) 1) 2 0) "m²")))
      					    (if (> area 10000) (setq pclareas (strcat (rtos (*  (/ (/ area 10000) 0.001) 0.001) 2 3) "ha")))
					    (if (> area 100000) (setq pclareas (strcat (rtos (*  (/ (/ area 10000) 0.01) 0.01) 2 2) "ha")))
					    (if (> area 1000000) (setq pclareas (strcat (rtos (*  (/ (/ area 10000) 0.1) 0.1) 2 1) "ha")))
					    (if (> area 10000000) (setq pclareas (strcat (rtos (*  (/ (/ area 10000) 1) 1) 2 0) "ha")))
                                            (if (> area 100000000) (setq pclareas (strcat (rtos (*  (/ (/ area 1000000) 0.1) 0.1) 2 1) "km²")))
                                            (if (> area 1000000000) (setq pclareas (strcat (rtos (*  (/ (/ area 1000000) 1) 1) 2 0) "km²")))
      

					    (if (/= pclclass "Road")(setq areapos (polar lotc (* 1.5 pi) (* th 2.5)))(setq areapos (polar lotc (* 1.5 pi) (* th 5))))
					    (if (= pcltype "Part" ) (setq pclareas (strcat "(" pclareas ")")))
					    (SETVAR "CELWEIGHT" 35)
					    (COMMAND "TEXT" "J" "BC" areapos (* TH 1.4) "90"  pclareas )
					    ))
       (setvar "dimzin" 8)

				 ;add create road to table
				  (if (and(or (= pclclass "Reserve")(= pclclass "Road"))(= pclstate "created"))(progn
  (COMMAND "TEXT" "J" "BL" RDLP 2.5 "90"  pclname )
(COMMAND "TEXT" "J" "BL" (LIST (+ (CAR RDLP) 37)(CADR RDLP)) 2.5 "90" PCLOWNER )
  (SETQ RDLP (LIST (CAR RDLP)(- (CADR RDLP) 4)))
  ))
					    
					    ;draw lot info
(setq lotdesc pclname);standard lot

       ;multipart lot
       (if (= pcltype "Part") (progn
				(setq -pos1 (vl-string-position 45 pclname 0))
			        (setq pclname  (substr pclname 1 -pos1))
				(if (and (/= pclclass "Easement")(/= pclclass "Restriction"))(setq lotdesc (strcat "PT " pclname)))
				;(setq pclname (substr pclname 1 (- (strlen pclname) 1)))
				))
      ; road or easement lot 
       (if (or (= pclclass "Road")(or (= pclclass "Easement")(= pclclass "Restriction"))) (progn
                                     (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" pcldesc &pos )) nil) (setq pcldesc (vl-string-subst "&" "&amp;"  pcldesc &pos)
										      &pos (+ &pos 4)))
				 (setq lotdesc pcldesc)
				 
				 ))
(setq THF 2)
       (SETQ CELLWEIGHT 50)
       (if (or (= pclclass "Easement")(= pclclass "Restriction"))(progn  (setq THF 1)
					(setq CELLWEIGHT -1)
				    
				      (setq lotdesc pclname)
				      
				   );p easement
    
	 (setq THF 2 ;else
	       CELLWEIGHT 50)
	 );if easement

       (if (= pclstate "adjoining") (setq cellweight -1))

	(SETVAR "CELWEIGHT" CELLWEIGHT)

				 
       (if (= pclstate "created")(progn
				   (setq slashpos1 (vl-string-position 92 lotdesc 0))
                 (setq lotdesc (substr lotdesc 1 slashpos1))
				   ))
				 
	;label road number if created road
       (if (and (= pclclass "Road")(= pclstate "created")) (COMMAND "TEXT" "J" "BC" (polar lotc (* 1.5 pi) (* th 2.5)) (* TH 1.4) "90" pclname))

	 ;normal lot/road labeller
	 (COMMAND "TEXT" "J" "BC" lotc (* TH THF) "90" lotdesc)
				 
				     
			 
       
 


       (if (/= pcluse "")(progn
			   (setq usepos (polar lotc (* 1.5 pi) (* th 5.5)))
			   (COMMAND "TEXT" "J" "BC" usepos (* TH THF) "90" pcluse)
			   )
	 )

			   (SETVAR "CELWEIGHT" -1);reset cellweight
			   
       (if (or (= pclclass "Road")(or (= pclclass "Easement")(= pclclass "Restriction")))  (setq roadname (entget (entlast))))
				 

       
  
					   (if (and (= pclstate "existing")(= pclclass "Road")) (setvar "CLAYER" "Adjoining Boundary")(SETVAR "CLAYER"  "Lot Definitions" ))
       (IF (and (= pclstate "adjoining")(or (= pclclass "Easement")(= pclclass "Restriction")))(setvar "CELTYPE" "EASEMENT"))

     

       ;do until end of parcel


        (while(= (vl-string-search "</Parcel>" linetext ) nil)( progn

       
      (linereader);coordgeom/title/address

      (if (vl-string-search "<CoordGeom " linetext )(progn
						       

	(linereader);line/arc/irregular line
       (setq ptlist nil)
       (while(= (vl-string-search "</CoordGeom>" linetext ) nil)( progn

								  

								  
 (if (/= (vl-string-search "<IrregularLine" linetext ) nil)(progn
		 (if (/= (setq stringpos (vl-string-search "desc" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
(setq irdesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
)(setq irdesc ""))

	(princ (strcat "\n" linetext))	 

		 
		 (linereader);start point
		 (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
(setq lp1 (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))))
		 (if (= (vl-string-search "/>" linetext) nil) (linereader));read closer


		 
		(linereader);end point
		  (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
(setq lp2 (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))))
		 (if (= (vl-string-search "/>" linetext) nil) (linereader));read closer

		 (setq irlinetest (strcat lp1 "-" lp2))
		 (setq irlinetestrev (strcat lp2 "-" lp1))
		 (setq irplotted "N")
		 (if (or (member irlinetest irlinetestlist)(member irlinetestrev irlinetestlist))(setq irplotted "Y"))

		 
		 (linereader);2d point list

		  (if (= importcgpoint "fromobs")(progn
		    
		    ;MAKE SHIFT BASED ON START POINT
		    (setq remainlist (member lp1 cgpointlist))
                    (setq lp1c (cadr remainlist))
		    	(setq ,pos1 (vl-string-position 44 lp1c 0))
                 (setq speast (atof (substr lp1c 1 ,pos1)))
      		 (setq spnorth (atof (substr lp1c (+ ,pos1 2) 50)))
		    		    
	        ))

		 (setq 2dpntlist "")
		 (setq >pos (vl-string-position 62 linetext))
		 
		 (if (= (vl-string-search "</PntList2D>" linetext ) nil)(progn
										
		 (while (= (vl-string-search "</PntList2D>" linetext ) nil)(progn
										(setq 2dpntlist (strcat 2dpntlist (substr linetext (+ >pos 2) 5000000) " "))
										(linereader)
										
										));while no <

		 (setq <pos (vl-string-position 60 linetext))
		 (if (/= <pos 0)(setq 2dpntlist (strcat 2dpntlist (substr linetext 1 (- <pos 1)))))
         	 (setq 2dpntlist (strcat 2dpntlist " "))

										);p if no <
		 (progn
		  (setq <pos (vl-string-position 60 linetext >pos ))
		 (setq 2dpntlist (strcat (substr linetext (+ >pos 2) (- (- <pos >pos) 1))))
		 ));if is < in line 1
         	 (setq 2dpntlist (strcat 2dpntlist " "))

		  ;get rid of tabs
	

		 (setq l2dpntlist "A")
		(while (/= l2dpntlist 2dpntlist) (setq l2dpntlist 2dpntlist
							2dpntlist (vl-string-subst " " "\t" 2dpntlist 1)))

		 	 
		 ;get rid of double spaces

		 (setq l2dpntlist "A")
		(while (/= l2dpntlist 2dpntlist) (setq l2dpntlist 2dpntlist
							2dpntlist (vl-string-subst " " "  " 2dpntlist 1)))

		 
							
		 (setq spcpos -1)
		 (setq spcpos1 1)
		 (setq ircolist (list))
		 (while (/=  (setq spcpos1 (vl-string-search " " 2dpntlist spcpos1 )) nil)
		   (progn
		   (setq ircolist (append ircolist (list (substr 2dpntlist (+ spcpos 2) (- (- spcpos1 spcpos) 1)))))
		   (setq spcpos spcpos1 )
		   (setq spcpos1 (+ spcpos1 1))
		   ));p and while space found 

		   (setq p1n (nth 0 ircolist))
		   (setq p1e (nth 1 ircolist))

(if (= importcgpoint "fromobs")(progn
				 (setq shifte (- speast (atof p1e)))
				 (setq shiftn (- spnorth (atof p1n)))
				 )
;else make shift 0
  (setq shifte 0
	shiftn 0)
  )
  



		 (setq lp1c (strcat (rtos (+ shifte (atof p1e)) 2 9) "," (rtos (+ shiftn (atof p1n)) 2 9)))
		 
		 ;(if (and (/= pclclass "Road")(/= pclclass "Easement")(/= pclstate "adjoining")(= (member lp1c poplist) nil))(setq poplist (append poplist (list lp1c))))

		 (setq irplotlist (append irplotlist (list ircolist)(list irplotted)))

		     
(if (> (length ircolist) 4)(progn;if list is more than 2 points		
(setq mpcount (+ (* 2 (fix (/ (/ (length ircolist) 2) 2))) 1))
		 (setq mp1e (nth mpcount ircolist))
		 (setq mp1n (nth (- mpcount 1)ircolist))
		 (setq mp2n (nth (+ mpcount 1)ircolist))
		 (setq mp2e (nth (+ mpcount 2)ircolist))
		 (setq mp1 (list (+ (atof mp1e) shifte) (+ (atof mp1n) shiftn)))
		 (setq mp2 (list (+ (atof mp2e) shifte) (+ (atof mp2n) shiftn)))
		 (setq mp (list (/ (+ (car mp1)(car mP2)) 2) (/ (+ (cadr mp1)(cadr mp2)) 2)))
)
  (progn;else
    		 (setq mp1e (nth 1 ircolist))
		 (setq mp1n (nth 0 ircolist))
		 (setq mp2n (nth (- (length ircolist) 2) ircolist))
		 (setq mp2e (nth (- (length ircolist) 1) ircolist))
		 (setq mp1 (list (+ (atof mp1e) shifte) (+ (atof mp1n) shiftn)))
		 (setq mp2 (list (+ (atof mp2e) shifte) (+ (atof mp2n) shiftn)))
		 (setq mp (list (/ (+ (car mp1)(car mP2)) 2) (/ (+ (cadr mp1)(cadr mp2)) 2)))
))
    
		 (setq mprot (angle mp1 mp2))
		 (setq mprot90 (+ mprot (* 0.5 pi)))
		 (if (/= lotc nil)(progn
				    (setq lotcext (polar lotc mprot90 100))
				    (setq p5 (inters mp1 mp2 lotc lotcext nil))
				    (setq mprot90 (angle lotc p5))
				    ))
						 
		
		 (if (= pclstate "adjoining")(setq textoff -2)(setq textoff 2));always put outside proposed lot
		 (SETQ 1POS (POLAR mp mprot90 (* TH textoff)))
                 (IF (AND (> mprot  (* 0.5 pi)) (< mprot (* 1.5 pi)))(setq mprot (+ mprot pi))) ;if text is upsidedown reverse rotation


		     (setq irtextlist (append irtextlist (list 1pos )(list mprot)(list irdesc)(list irplotted)))
                 ;(SETVAR "CLAYER"  "Drafting" )
		 ;(COMMAND "TEXT" "J" "MC" 1pos TH (ANGTOS mprot 1 4) irdesc)
                 ;(if (/= pclstate "adjoining") (SETVAR "CLAYER"  "Lot Definitions" )(setvar "CLAYER" "Adjoining Boundary"))
(SETQ 1POS (POLAR mp mprot90 (* TH 5)))

		     (SETQ irlinetestlist (APPEND irlinetestlist (LIST irlinetest) 1pos (list (angle mp1 mp2)) (LIST irlinetestrev) 1pos (list (angle mp2 mp1))))
		 
		    	
		   
;irregular plotter and labeller moved to after lot import due to entlast problems

		    
		   
	 (setq ircount 0)
		 (repeat (- (/ (length ircolist) 2) 1)
		   (setq p1n (atof (nth ircount ircolist)))
		   (setq p1e (atof (nth (+ ircount 1) ircolist)))
		   (setq p2n (atof (nth (+ ircount 2) ircolist)))
		   (setq p2e (atof (nth (+ ircount 3) ircolist)))
		   (setq p1 (list (+ shifte p1e)(+ shiftn p1n)))
		   (setq p2 (list (+ shifte p2e)(+ shiftn p2n)))

		    (command "line" p1 p2 "")
		     (SETQ RMB (ENTLAST))
		     (SSADD RMB ENTSS)
		   
		   (setq ircount (+ ircount 2))
		   );R

		 
                  (linereader);read irregular line end
		  
		 ));p and if irregular line

								  
       

       (if (/= (vl-string-search "<Line" linetext ) nil)(progn

							  ;check for building boundary
							    (if (/= (setq stringpos (vl-string-search "desc" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq blddesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq blddesc ""))
							  
							  (linereader)
							   (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
(setq lp1 (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
(setq remainlist (member lp1 cgpointlist))
(setq lp1c (cadr remainlist))
(setq ptlist (append ptlist (list lp1c)))
(if (= (vl-string-search "/>" linetext ) nil) (linereader))
;(if (and (/= pclclass "Road")(/= pclclass "Easement")(/= pclclass "Restriction")(/= pclstate "adjoining")(= (member lp1c poplist) nil))(setq poplist (append poplist (list lp1c))))
))
							   (linereader)
							   (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
(setq lp2 (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
(setq remainlist (member lp2 cgpointlist))
(setq lp2c (cadr remainlist))
))
							  (if (= (vl-string-search "/>" linetext) nil) (linereader))

(if (or (= pclclass "Easement")(= pclclass "Restriction"))(setq easementlist (append easementlist (list (strcat lp1c "," lp2c))(list (strcat lp2c "," lp1c)))));add line and reverse to easementlist
(if (= pclstate "extinguished") (setq extglist (append extglist (list (strcat lp1c "," lp2c))(list (strcat lp2c "," lp1c)))))
							  (if (= pclclass "Road")(setq otherlines (append otherlines (list (strcat lp1c "," lp2c))(list (strcat lp2c "," lp1c)))));add line and reverse to easementlist
(if (= pclclass "Lot")(setq otherlines (append otherlines (list (strcat lp1c "," lp2c))(list (strcat lp2c "," lp1c)))));add line and reverse to easementlist							  


							 (linereader);</line>
							  ;for the time being draw a line
							  (command "line" lp1c lp2c "")
							  (SETQ RMB (ENTLAST))
							  (SSADD RMB ENTSS)

							;DRAW EXISTING EASEMENT ON LOT AS POLYLINE
							  (if (and (or (= pclclass "Easement")(= pclclass "Restriction")) (= pclstate "existing"))
   							(progn
     							(setvar "clayer" "Easement")
     							(command "line" lp1c lp2c "")
     							(SETQ RMB (ENTLAST))
     							(SSADD RMB EASESS)	      
     							))

							  (if (and (/= blddesc "")
								   (= (member (strcat lp1c "-" lp2c) builddrawn) nil)
								   (= (member (strcat lp2c "-" lp1c) builddrawn) nil)
								   (= pclclass "Lot")
								   )
							    (progn
							      (if (= blddesc "Median")(setvar "clayer" "Boundary Wall Median"))
							      (if (= blddesc "Other")(setvar "clayer" "Boundary Wall Other"))
							      (if (= blddesc "Interior Face")(setvar "clayer" "Boundary Wall Interior"))
							      (if (= blddesc "Exterior Face")(setvar "clayer" "Boundary Wall Exterior"))
							      (command "line" lp1c lp2c "")
							      (setq builddrawn (append builddrawn (list (strcat lp1c "-" lp2c)(strcat lp2c "-" lp1c))))
							      ))

							 
							      

							  
							  ));p and if line
 
   

      (if (/= (vl-string-search "<Curve" linetext ) nil)(progn
							  ;check for building boundary
							    (if (/= (setq stringpos (vl-string-search "desc" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq blddesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq blddesc ""))
							  
							   (if (/= (setq stringpos (vl-string-search "rot" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 5)))(setq curverot (substr linetext (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4))))))
							 (if (/= (setq stringpos (vl-string-search "radius" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))(setq radius (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))))
							  (linereader)
							   (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
(setq cp1 (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
(setq remainlist (member cp1 cgpointlist))
(setq lp1c (cadr remainlist))
(setq ptlist (append ptlist (list lp1c)))
;(if (and (/= pclclass "Road")(/= pclclass "Easement")(/= pclstate "adjoining")(= (member lp1c poplist) nil))(setq poplist (append poplist (list lp1c))))
))
							  (if (= (vl-string-search "/>" linetext) nil) (linereader))
							  
							   (linereader)
							   (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
(setq curvecen (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
(setq remainlist (member curvecen cgpointlist))
(setq curvecenc (cadr remainlist))
))
							  (if (= (vl-string-search "/>" linetext) nil) (linereader))
							  
							  (linereader)
							   (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
(setq cp2 (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
(setq remainlist (member cp2 cgpointlist))
(setq lp2c (cadr remainlist))
))
							  (if (= (vl-string-search "/>" linetext) nil) (linereader))
							  
							  (linereader)

		(setq ,pos1 (vl-string-position 44 lp1c 0))
                 (setq east (atof (substr lp1c 1 ,pos1)))
      		 (setq north (atof (substr lp1c (+ ,pos1 2) 50)))
					   (setq tp1 (list east north))
                                           (setq p1 (list east north))
  (setq ,pos1 (vl-string-position 44 lp2c 0))
                 (setq east (atof (substr lp2c 1 ,pos1)))
      		 (setq north (atof (substr lp2c (+ ,pos1 2) 50)))
					   (setq tp2 (list east north))
                                           (setq p2 (list east north))

(setq ,pos1 (vl-string-position 44 curvecenc 0))
                 (setq east (atof (substr curvecenc 1 ,pos1)))
      		 (setq north (atof (substr curvecenc (+ ,pos1 2) 50)))
					   (setq cc1 (list east north))
                                           (setq c1 (list east north))

							  (setq ang1 (angle cc1 p1))
							  (setq ang2 (angle cc1 p2))
							  (if (= curverot "cw")(setq O (- ANG1 ANG2)))
							  (if (= curverot "ccw")(setq O (- ANG2 ANG1)))
							  
  (IF (< O 0) (SETQ O (+ O (* PI 2))))
  	   (setq arclength (rtos ( *  (atof radius) O) 2 3))
							  
	    (setq digchaz (angle p1 p2))

;calc arc internal angle
	       (SETQ MAST (SQRT (- (* (atof RADIUS) (atof RADIUS)) (* (/ (distance p1 p2) 2)(/ (distance p1 p2) 2 )))))
  (SETQ O (* 2 (ATAN (/ (/ (distance p1 p2) 2) MAST))))
	    (setq remhalfO  (- (* 0.5 pi) (/ O 2)))
	    ;calc bearing from p1 to arc centre (watching for bulbous arcs)
	    (if (and (= curverot "ccw") (<= (atof arclength) (* pi (atof radius))))(setq raybearing (+  digchaz  remhalfO)))
	    (IF (and (= curverot "cw") (<= (atof arclength) (* pi (atof radius))))(setq raybearing (-  digchaz  remhalfO)))
	    (IF (and (= curverot "ccw") (> (atof arclength) (* pi (atof radius))))(setq raybearing (-  digchaz  remhalfO)))
	    (if (and (= curverot "cw") (> (atof arclength) (* pi (atof radius))))(setq raybearing (+  digchaz  remhalfO)))
	    
	      ;CONVERT TO ANTI CLOCKWISE AND EAST ANGLE
  ;(SETQ raybearing (+ (* -1 raybearing) (* 0.5 PI)))

	       ;calc curve centre point
	    (setq curvecen (polar p1 raybearing (atof radius)))
	    (setq curvecenc (strcat (rtos (car curvecen) 2 9) "," (rtos (cadr curvecen) 2 9)))

(if (or (= pclclass "Easement")(= pclclass "Restriction"))(setq easementlist (append easementlist (list (strcat lp1c "," lp2c))(list (strcat lp2c "," lp1c)))));add line and reverse to easementlist
(if (= pclstate "extinguished") (setq extglist (append extglist (list (strcat lp1c "," lp2c))(list (strcat lp2c "," lp1c)))))
							  (if (= pclclass "Road")(setq otherlines (append otherlines (list (strcat lp1c "," lp2c))(list (strcat lp2c "," lp1c)))));add line and reverse to not eassementlist
(if (= pclclass "Lot")(setq otherlines (append otherlines (list (strcat lp1c "," lp2c))(list (strcat lp2c "," lp1c)))));add line and reverse to not easementlist							  


							 (if (= curverot "ccw") (command "arc" "c" curvecenc lp1c lp2c)(command "arc" "c" curvecenc lp2c lp1c))
							  (SETQ RMB (ENTLAST))
							  (SSADD RMB ENTSS)

							  ;DRAW LINES FOR EXISTING EASEMENT
							  (if (and (or (= pclclass "Easement")(= pclclass "Restriction")) (= pclstate "existing"))
   							(progn
     							(setvar "clayer" "Easement")
     							(if (= curverot "ccw") (command "arc" "c" curvecenc lp1c lp2c)(command "arc" "c" curvecenc lp2c lp1c))
     							(SETQ RMB (ENTLAST))
     							(SSADD RMB EASESS)	      
     							))

							 (if (and (/= blddesc "")
								  (= (member (strcat lp1c "-" lp2c) builddrawn) nil);not already drawn
								  (= (member (strcat lp2c "-" lp1c) builddrawn) nil)
								  (= pclclass "Lot");not common property or a road
								  )
							  
							    (progn
							      (if (= blddesc "Median")(setvar "clayer" "Boundary Wall Median"))
							      (if (= blddesc "Other")(setvar "clayer" "Boundary Wall Other"))
							      (if (= blddesc "Interior Face")(setvar "clayer" "Boundary Wall Interior"))
							      (if (= blddesc "Exterior Face")(setvar "clayer" "Boundary Wall Exterior"))
							      (if (= curverot "ccw") (command "arc" "c" curvecenc lp1c lp2c)(command "arc" "c" curvecenc lp2c lp1c))
							      (setq builddrawn (append builddrawn (list (strcat lp1c "-" lp2c)(strcat lp2c "-" lp1c))))
							      ))


							

							  
							 ));p and if curve

	(IF (and (= pclstate "adjoining")(or (= pclclass "Easement")(= pclclass "Restriction")))(setvar "CELTYPE" "ByLayer"));reset variable for adjoining lots

					    (linereader);read line curve or end of coordinate geometery



       	));p and while </CoordGeom>					  

								  (setq ptlist (append ptlist (list lp2c)))

       ;join all exsiting easements into a polyline so they dont get exported
       (if (and (or (= pclclass "Easement")(= pclclass "Restriction")) (= pclstate "existing"))(command "pedit" "m" easess "" "y" "j" "" ""))
       
       ;join all plotted lot defintions in to a polyline
					    (command "pedit" "m" entss "" "y" "j" "" "")
       
))

      	


					    


          ;VIC added read title and plan info


				
					 ; (linereader);read nextline

     
      
;VIC store extingushed lots for titleblock
        (if (and (= (member pclname extplist) nil) (= pclstate "extinguished"))(setq extplist (append extplist (list pclname))))

      
      (if (/= (vl-string-search "<Title" linetext ) nil)(progn;check for title string
							  							       
							   (if (/= (setq stringpos (vl-string-search "name" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq titlename (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq titlename ""))
							   (if (/= (setq stringpos (vl-string-search "titleType" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 11)))(setq titletype (substr linetext (+ stringpos 12) (-(- wwpos 1)(+ stringpos 10)))))(setq titletype ""))
							

							  	;REMOVE PLAN NAME FOR CREATED tiles
				 (if (vl-string-search planname titlename)(progn
		 (setq linetext (vl-string-subst ""  (strcat "\\" planname) linetext))
		 (SETQ titlename (vl-string-subst ""  (strcat "\\" planname) titlename))
		
		))	    

							  (setq titles (strcat titles linetext))

							  (if (= titletype "Restriction Benefit")(progn
								(if (/= (setq remlist (member titlename rbenlist)) nil)
								  (progn;if already exists
								    (setq currentlots (nth 1 remlist))
								    
								    (setq newlots (strcat currentlots "," pclname))
								    (setq rbenlist (replaceitem  (+ (- (length rbenlist)(length remlist))2) newlots rbenlist))
								    )
								  (progn;else
								    (setq rbenlist (append rbenlist (list titlename pclname)))
								    )
								  );if already in benlist
								));p&if restriction benifit

							   (if (= titletype "Restriction Burden")(progn
								(if (/= (setq remlist (member titlename rburlist)) nil)
								  (progn;if already exists
								    (setq currentlots (nth 1 remlist))
								    (setq newlots (strcat currentlots "," pclname))
								    (setq rburlist (replaceitem  (+ (- (length rburlist)(length remlist)) 2) newlots rburlist))
								    )
								  (progn;else
								    (setq rburlist (append rburlist (list titlename pclname)))
								    )
								  );if already in benlist
								));p&if restriction burden
									  


							   ;VIC store extingusihed lots for titleblock

		(if (and (= (member titlename exttlist) nil) (= pclstate "extinguished")(/= titletype "Depth Limitation"))(setq exttlist (append exttlist (list titlename))))
							   ;(if (= titletype "Depth Limitation")(setq dllist (append dllist (list titlename))))
																	  
							;   (linereader);read nextline

							   
							   ))

       (if (/= (vl-string-search "<LocationAddress" linetext ) nil)(progn;check for address string
							       
							   (if (/= (setq stringpos (vl-string-search "addressType" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 13)))(setq addresstype (substr linetext (+ stringpos 14) (-(- wwpos 1)(+ stringpos 12)))))(setq addresstype ""))
							   (if (/= (setq stringpos (vl-string-search "numberFirst" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 13)))(setq numberfirst (substr linetext (+ stringpos 14) (-(- wwpos 1)(+ stringpos 12)))))(setq numberfirst ""))
							   (if (/= (setq stringpos (vl-string-search "numberSuffixFirst" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 19)))(setq numberSuffixFirst (substr linetext (+ stringpos 20) (-(- wwpos 1)(+ stringpos 18)))))(setq numberSuffixFirst ""))
							     (if (/= (setq stringpos (vl-string-search "numberLast" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 12)))(setq numberLast (substr linetext (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11)))))(setq numberLast ""))
							   (if (/= (setq stringpos (vl-string-search "numberSuffixLast" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 18)))(setq numberSuffixLast (substr linetext (+ stringpos 19) (-(- wwpos 1)(+ stringpos 17)))))(setq numberSuffixLast ""))
							   (if (/= (setq stringpos (vl-string-search "flatNumber" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 12)))(setq flatNumber (substr linetext (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11)))))(setq flatNumber ""))
							   (if (/= (setq stringpos (vl-string-search "flatType" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 10)))(setq flatType (substr linetext (+ stringpos 11) (-(- wwpos 1)(+ stringpos 9)))))(setq flatType ""))
							   (if (/= (setq stringpos (vl-string-search "floorLevelNumber" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 18)))(setq floorLevelNumber (substr linetext (+ stringpos 19) (-(- wwpos 1)(+ stringpos 17)))))(setq floorLevelNumber ""))
							   (if (/= (setq stringpos (vl-string-search "floorLevelType" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 16)))(setq floorLevelType (substr linetext (+ stringpos 17) (-(- wwpos 1)(+ stringpos 15)))))(setq floorLevelType ""))

							   
							   (linereader)


							   (if (/= (setq stringpos (vl-string-search "roadName=" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 10)))(setq roadName (substr linetext (+ stringpos 11) (-(- wwpos 1)(+ stringpos 9)))))(setq roadName ""))
							    (if (/= (setq stringpos (vl-string-search "roadNameSuffix" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 10)))(setq roadNameSuffix (substr linetext (+ stringpos 11) (-(- wwpos 1)(+ stringpos 9)))))(setq roadNameSuffix ""))
							   (if (/= (setq stringpos (vl-string-search "roadNameType" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 14)))(setq roadNameType (substr linetext (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq roadNameType ""))
							   (if (/= (setq stringpos (vl-string-search "roadType" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 10)))(setq roadType (substr linetext (+ stringpos 11) (-(- wwpos 1)(+ stringpos 9)))))(setq roadType ""))

							   (linereader);read nextline

							   (if (/= (setq stringpos (vl-string-search "adminAreaName" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))(setq adminAreaName (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq adminAreaName ""))
							   (if (/= (setq stringpos (vl-string-search "adminAreaType" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))(setq adminAreaType (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq adminAreaType ""))
							   (if (/= (setq stringpos (vl-string-search "adminAreaCode" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))(setq adminAreaCode (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq adminAreaCode ""))

							   (setq comboaddress (strcat numberfirst numbersuffixfirst " " roadname " " roadnametype " " adminareaname " " adminareacode))
							   (if (and (= (member comboaddress extalist) nil) (= pclstate "extinguished"))(setq extalist (append extalist (list comboaddress))))
							   (setq smalladdress (strcat "<smalladdress=\"" flatNumber ","
										     flatType ","
										     floorLevelNumber ","
										     floorLevelType ","
										     numberfirst ","
										     numberSuffixFirst ","
										     numberLast ","
										     numberSuffixLast ","
										     roadName ","
										     roadnamesuffix ","
										     roadnametype ","
										     roadtype ","
										     adminareaname ","
										     adminAreaCode "\"/>"))
							   (linereader);read nextline (</LocationAddress>
						;	   (linereader);read nextline 

							  
							   )
	 );if location



       


    


     ;  (linereader)

       ));while not </parcel>

  
					   ; (linereader);read next parcel or end of parcels

							       

       
      


       
       (if (/= pcldesc "")(setq desc (strcat " desc=\"" pcldesc "\""))(setq desc ""))
       (if (/= pclarea "")(setq areas (strcat " area=\"" pclarea "\""))(setq areas ""))
       (if (/= pclformat "")(setq pclformats (strcat " parcelFormat=\"" pclformat "\""))(setq pclformats ""))
       	(if (/= pclowner "")(setq pclowners (strcat " owner=\"" pclowner "\""))(setq pclowners ""))
       
  (SETQ LTINFO (STRCAT "  <Parcel name=\"" pclname "\"" desc  " class=\"" pclclass "\" state=\"" pclstate "\" parcelType=\"" pcltype "\"" pclformats pclowners
		       areas ">" titles smalladdress "!" (rtos (cadr lotc) 2 6 ) " " (rtos (car lotc) 2 6)))
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))


       (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)


				 	;plot irregular lines
					    (setq ircount1 0)
		(repeat (/ (length irplotlist) 2)
		  
		  (setq ircolist (nth ircount1 irplotlist))
		  (setq irplotted (nth (+ ircount1 1)  irplotlist))
		 (setq irss (ssadd))
		 (setq ircount 0)

		 
		  (IF (= IRPLOTTED "N")(PROGN
		 (repeat (- (/ (length ircolist) 2) 1)
		   (setq p1n (atof (nth ircount ircolist)))
		   (setq p1e (atof (nth (+ ircount 1) ircolist)))
		   (setq p2n (atof (nth (+ ircount 2) ircolist)))
		   (setq p2e (atof (nth (+ ircount 3) ircolist)))
		   (setq p1 (list (+ shifte p1e)(+ shiftn p1n)))
		   (setq p2 (list (+ shifte p2e)(+ shiftn p2n)))

		   (setvar "clayer" "Irregular Boundary")
		   (IF (= pclstate "adjoining" )(SETVAR "CELWEIGHT" 25))
		   (command "pline" p1 p2 "")
		   (IF (= pclstate "adjoining" )(SETVAR "CELWEIGHT" -1))
		       (SETQ RMB (ENTLAST))
		     (SSADD RMB irss)
		   (if (and (/= (member pclclass interestlist) nil) (= pclstate "existing"))
   							(progn
     							(setvar "clayer" "Easement")
     							(command "line" p1 p2 "")
     							(SETQ RMB (ENTLAST))
     							(SSADD RMB EASESS)	      
     							))
		   
		   (setq ircount (+ ircount 2))
		   );R lenth ircolist
		 
		   
		 
		     (IF (> (LENGTH IRCOLIST) 4)
		       (command "pedit" "m" irss "" "j" "" "s" "");longer than 2 verticies make spline
		       )

		  (SETQ LTINFO irdesc)
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO)))))
     (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
		     
 
		   ));p&if  not plotted
		  (setq ircount1 (+ ircount1 2)) 
		  );r
      

	     (SETVAR "CLAYER"  "Drafting" )
					    
	    ;plot all irregualar line labels
	    (setq ircount 0)
		    (repeat (/ (length irtextlist) 4)
	  (if (= (nth (+ ircount 3) irtextlist) "N")(COMMAND "TEXT" "J" "MC" (nth ircount irtextlist) TH (ANGTOS (nth (+ ircount 1) irtextlist) 1 4) (nth (+ ircount 2) irtextlist)))
		   (setq ircount (+ ircount 4))
		      )



       
       ;road name rotator
       (if (or (= pclclass "Road")
	       (and (= pclclass "Easement")(= pclformat "Geometry")(= pcltype "Single"))
	       (and (= pclclass "Easement")(= pclformat "Geometry")(= pcltype "Part"))
	       (and (= pclclass "Restriction")(= pclformat "Geometry")(= pcltype "Single"))
	       (and (= pclclass "Restriction")(= pclformat "Geometry")(= pcltype "Part"))
	       )
	 (progn
		(setq nearang pi)					    
		(setq count 0)
		(setq minoff 100000000000000000000000000000000000000)
		(setq ptlist (append ptlist (list(nth 0 ptlist))))
(repeat (- (length ptlist)2 )

  (setq op1 (nth count ptlist))
  (setq op2 (nth (+ count 1) ptlist))
  (setq op3 (nth (+ count 2) ptlist))

  (setq ,pos1 (vl-string-position 44 op1 0))
(setq op1 (list (atof (substr op1 1 ,pos1)) (atof (substr op1 (+ ,pos1 2) 50))))
  (setq ,pos1 (vl-string-position 44 op2 0))
(setq op2 (list (atof (substr op2 1 ,pos1)) (atof (substr op2 (+ ,pos1 2) 50))))
  (setq ,pos1 (vl-string-position 44 op3 0))
(setq op3 (list (atof (substr op3 1 ,pos1)) (atof (substr op3 (+ ,pos1 2) 50))))

  ;check line one
;check offset to line
  (SETQ ANG (ANGLE oP1 oP2))
  (SETQ CANG (+ ANG (/ PI 2)))
  (SETQ P4 (POLAR lotc CANG 1000))
  (SETQ P6 (POLAR lotc (+ CANG PI) 2000))
  (IF (SETQ P5 (INTERS oP1 oP2 P6 P4 ))(PROGN

					 (SETQ OFF (DISTANCE lotc P5))

    (if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang cang)
      );if
    
  ));p & if inters


  

    ;check inside deflection angle
    (setq ang2 (angle op2 op3))
    (setq cang2 (+ ang (/ pi 2)))
    (setq ltopang (angle op2 lotc))
    (setq defl (- ang2 ang))
(if (/= defl 0);check for straight line
  (progn
    (if (< defl pi)(progn
		     (setq ray1 (- ang (/ pi 2)))
		     ;(if (< ray1 0) (setq ray1 (+ ray1 (* 2 pi))))
		     (setq ray2 (- ang2 (/ pi 2)))
		     ;(if (< ray2 0) (setq ray2 (+ ray2 (* 2pi))))
		     ));p and if less than pi
    (if (> defl pi)(progn
		     (setq ray1 (+ ang (/ pi 2)))
		     ;(if (> ray1 (* 2 pi)(setq ray1 (- ray1 (* 2 pi)))))
		     (setq ray2 (+ ang (/ pi 2)))
		     ;(if (> ray2 (* 2 pi)(setq ray2 (- ray2 (* 2 pi)))))
		     ));p and if greater than pi

    (if (or (and (> ltopang ray1) (< ltopang ray2))(and (> ltopang ray2)(< ltopang ray1)));check ot see if inside deflection squares **** needs testing later
      (progn
	(setq off (distance lotc op2))
(if (< OFF (ABS MINOFF))(SETQ minoff (abs off)
				  nearang ltopang)
      );if
	));p and if in defl sqr
   ));p and if not straight line

  (setq count (+ count 1))
    );repeat
      
				;using this info change the road name text rotation to the angle of the line
		(setq rrot (+ nearang (/ pi 2)))
		(if (> rrot (* 2 pi))(setq rrot (- rrot (* 2 pi))))
		(IF (AND (> rrot  (* 0.5 pi)) (< rrot (* 1.5 pi)))(setq rrot (+ rrot pi))) ;if text is upsidedown reverse rotation
					  
		
    
  (SETQ	roadname (subst (cons 50  rrot);SUB IN NEW POINT 2 HEIGHT
		     (assoc 50 roadname)
		     roadname
	      )
  )
  
	;	  (SETQ	roadname (subst (cons 73  2);SUB IN NEW POINT 2 HEIGHT
	;	     (assoc 73 roadname)
	;	     roadname
	      ;)
  ;)
  (ENTMOD roadname)
	   (if (= pclclass "Road");if road add extra label in Darfting AFR
	     (progn
	       (SETQ roadname (subst (cons 8  "Drafting AFR")(assoc 8 roadname) roadname))
	       (SETQ roadname (subst (cons 50  rrot )(assoc 50 roadname) roadname))
		(ENTMAKE roadname)
	       )
	     )
		
		     
	      
  

				));p & if road
    
    ));p&f not a single liner restriction

  
));p & if multipart


        (linereader);read next parcel or end of parcels


       
   ));p and while not </Parcels>

  

  ;Sort out the easment line list by comparing the road and boundary lines to the easement lines
  (setq count 0)
  (if (> (length easementlist) 0)(progn
				 (repeat (length easementlist)
    (setq checkline (nth count easementlist))
    (if  (= (member checkline otherlines) nil) (setq finaleaselist (append finaleaselist (list checkline))))
    (setq count (+ count 1))
    )
				 );p
    (setq finaleaselist easementlist);else

    );if length of otherlines



  ;draw the restrictions now all titles have been imported

  (setq rcount 0)
  (princ (rtos (length resnamelist) 2 0))
  (if (> (length resnamelist) 0)
    (progn
    (repeat (/ (length resnamelist) 2)
    (progn
      (setq pclname (nth rcount resnamelist))
      (setq pcldesc (nth (+ rcount 1) resnamelist))

  (if (setq remlist (member pclname rbenlist))(setq ben (nth 1 remlist)))
								(if (setq remlist (member pclname rburlist))(setq bur (nth 1 remlist)))
										(setvar "CLAYER" "Admin Sheet")
										;(if (= pclstate "created")
										 ; (progn
										       (if (vl-string-position 44 ben)(setq ben (strcat "s " ben))(setq ben (strcat " " ben)))
										       (if (vl-string-position 44 bur)(setq bur (strcat "s " bur))(setq bur (strcat " " bur)))
										( command "text" rlp "3.5" "90" (strcat "Creation of Restriction " pclname) )
										(setq rlp (list (car rlp)(- (cadr rlp) 5)))
										( command "text" rlp "2.5" "90" (strcat "Land to Benefit: Lot" ben ) )
										(setq rlp (list (car rlp)(- (cadr rlp) 4)))
										( command "text" rlp "2.5" "90" (strcat "Land to Burden: Lot" bur ) )
										(setq rlp (list (car rlp)(- (cadr rlp) 4)))

										     (setq &pos 0)
					      	      (while (/=  (setq &pos (vl-string-search "&amp;" pcldesc &pos )) nil) (setq pcldesc (vl-string-subst "&" "&amp;"   pcldesc &pos)
										      &pos (+ &pos 1)))
				        (setq \pos 0)
					      	      (while (/=  (setq \pos (vl-string-search ( chr 92) pcldesc \pos )) nil) (setq pcldesc (vl-string-subst (strcat (chr 92) (chr 92)) (chr 92)   pcldesc \pos)
										      \pos (+ \pos 2)))
										    
					       (setq crlfpos 0)
					      	      (while (/=  (setq crlfpos (vl-string-search "&#xD;&#xA;" pcldesc crlfpos )) nil) (setq pcldesc (vl-string-subst (chr 10) "&#xD;&#xA;"   pcldesc crlfpos)
										      crlfpos (+ crlfpos 1)))

				        (setq crlfpos 0)
					      	      (while (/=  (setq crlfpos (vl-string-search "&#xA;" pcldesc crlfpos )) nil) (setq pcldesc (vl-string-subst (chr 10) "&#xA;"   pcldesc crlfpos)
										      crlfpos (+ crlfpos 1)))
					          					
	    (setq quotpos 0)
	(while (/=  (setq quotpos (vl-string-search "&quot;" pcldesc quotpos )) nil) (setq pcldesc (vl-string-subst "\"" "&quot;"  pcldesc 	quotpos)
										      quotpos (+ quotpos 1)))
					      (setq apos 0)
	(while (/=  (setq apos (vl-string-search "&apos;" pcldesc apos )) nil) (setq pcldesc (vl-string-subst "'" "&apos;"  pcldesc apos)
										      apos (+ apos 1)))
	    (setq apos 0)
	    	(while (/=  (setq apos (vl-string-search "&#176;" pcldesc apos )) nil) (setq pcldesc (vl-string-subst "°" "&#176;"  pcldesc apos)
										      apos (+ apos 1)))
										
										
										(command "mtext" rlp "h" "2.5" "@250,-10" pcldesc "")
										 (SETQ LTINFO2 pclname )
						    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO2)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)
										(setq rlp (list (car rlp) (- (cadr rlp) (+ 10(* 4  (/ (strlen pcldesc) 100))))))
										
										;));if created label
      (setq rcount (+ rcount 2))
      ))));rppif resnamelist exists


  

  ;read PLAN FEATURES if they exist-----------------------------------------------------------------

(princ "\nReading Plan Features")
  
  (close xmlfile)
   (setq xmlfile (open xmlfilen "r"))
(linereader)

  ;linefeed to planfeatures
 (while (and (= (vl-string-search "</LandXML>" linetext) nil)(= (vl-string-search "<PlanFeatures" linetext) nil)) ( progn 
 (linereader)
))
  
  (if (= (vl-string-search "</LandXML>" linetext) nil)(progn ;catch for no plan features
  

(linereader);read feature info line
  ;do until end of parcels
       (while (= (vl-string-search "</PlanFeatures>" linetext ) nil) ( progn

  (setq sp nil)
					  
;get parcel info
       (if (/= (setq stringpos (vl-string-search "name" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq pclname (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq pclname ""))
       (if (/= (setq stringpos (vl-string-search "desc" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq pcldesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq pcldesc ""))
(setq &pos 0)
  	      (while (/=  (setq &pos (vl-string-search "&amp;" pcldesc &pos )) nil) (setq pcldesc (vl-string-subst "&" "&amp;"  pcldesc &pos)
   										      &pos (+ &pos 1)))
;figure out layer
       (SETQ pclname (strcase pclname))
		(if (= (substr pclname 1 4) "WALL")(setq layer "Occupation Walls"))
		(if (= (substr pclname 1 4) "BRT-")(setq layer "Occupation Building Return"))
		(if (= (substr pclname 1 4) "TWAL")(setq layer "Occupation Timber Walls"))
		(if (= (substr pclname 1 4) "FEN-")(setq layer "Occupation Fences"))
       		(if (= (substr pclname 1 4) "KERB")(setq layer "Occupation Kerbs"))
		(if (= (substr pclname 1 4) "CNTL")(setq layer "Occupation Centreline"))
		(if (= (substr pclname 1 4) "RAIL")(setq layer "Occupation Railway"))
		(if (= (substr pclname 1 4) "RWAL")(setq layer "Occupation Rockwall"))
       		(if (= (substr pclname 1 4) "HDG-")(setq layer "Occupation Hedge"))
		(if (= (substr pclname 1 4) "OTH-")(setq layer "Occupation Other"))
                (if (= (substr pclname 1 4) "GATE")(setq layer "Occupation Gate"))
                (if (= (substr pclname 1 4) "NSMB")(setq layer "Occupation Not Fenced"))


       
		(if (= (substr pclname 1 4) "OFF-")(setq layer "Occupations"))
                (if (= (substr pclname 1 4) "CHAI")(setq layer "Occupations"
							 chainlist "00"
							 chaindist 0))
					    
  
					   (SETVAR "CLAYER"  layer )

       (linereader);coord geometery name - not needed yet

       ;do until end of parcel
      (linereader);line or arc
       (setq ptlist nil)
	 (setq geolength 0)
         (SETQ ENTSS (SSADD))
					    
       (while(= (vl-string-search "</CoordGeom>" linetext ) nil)( progn

								  
       

       (if (/= (vl-string-search "<Line" linetext ) nil)(progn
							   (if (/= (setq stringpos (vl-string-search "desc" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq ldesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq ldesc ""))
(setq &pos 0)

							  
							   
							 (linereader)
							  
							   (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
(setq lp1 (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
(setq remainlist (member lp1 cgpointlist))
(setq lp1c (cadr remainlist))
(setq ptlist (append ptlist (list lp1c)))
(if (= (vl-string-search "/>" linetext ) nil) (linereader))
))
							   (linereader)
							   (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
(setq lp2 (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
(setq remainlist (member lp2 cgpointlist))
(setq lp2c (cadr remainlist))
(if (= (vl-string-search "/>" linetext ) nil) (linereader))
))



							 (linereader);</line>
							  
							  ;for the time being draw a line
							  (command "line" lp1c lp2c "")
							  (SETQ RMB (ENTLAST))
							  (SSADD RMB ENTSS)
							  (setq geolength (+ geolength 1))
							  
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
   (setq p1 (CDR(ASSOC 10 sentlist)))
   (setq p2 (CDR(ASSOC 11 sentlist)))
							  ;if a chainage draw and add to chainage list
							  (if (and (= (substr pclname 1 4) "CHAI")(/= ldesc ""))
							    (progn

    (SETQ CP1 (TRANS  p1 0 1))
    (SETQ CP2 (TRANS  p2 0 1))
    (SETQ ANG (- (ANGLE CP1 CP2) (* 0.5 pi)))
    (SETQ DANG (* ANG (/ 180 PI)))

    (IF (AND (> DANG 90) (<= DANG 270)) (PROGN
					 (SETQ CP1 (TRANS P2 0 1))
					 (SETQ CP2 (TRANS p1 0 1))
					 (SETQ ANG (- (ANGLE CP1 CP2) (* 0.5 PI)))
                                         (SETQ DANG (* ANG (/ 180 PI)))
					 )
      )

    (setq chaindist (+ (atof ldesc) chaindist))
    
   (SETVAR "CLAYER"  "Drafting AFR" )

    
		 (if (= sp nil) (progn ;if first chainage add 00
				  (setq sp (cdr(assoc 10 sentlist)))
				  (setq 00pos (polar sp (- (angle cp1 cp2) (* 0.5 pi)) (* 0.7 TH)))
				  (command "insert" "VCH0" "_S" TH 00pos (angtos (ANGLE (trans p1 0 1)(trans p2 0 1)) 1 4))
				  ))
    
    (COMMAND "TEXT" "J" "ML" P2 TH (ANGTOS ANG 1 4) (rtos chaindist 2 3))

    (setq chainlist (strcat chainlist "," ldesc))
    

				(setq layer "Occupations")

    ));p&if chainage
							  

							  ));p and if line

      (if (/= (vl-string-search "<Curve" linetext ) nil)(progn
							   (if (/= (setq stringpos (vl-string-search "rot" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 5)))(setq curverot (substr linetext (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4))))))
							 (if (/= (setq stringpos (vl-string-search "radius" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))(setq radius (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))))
							  
							  (linereader)
							   (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
(setq cp1 (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
(setq remainlist (member cp1 cgpointlist))
(setq lp1c (cadr remainlist))
(setq ptlist (append ptlist (list lp1c)))
))
							   (linereader)
							   (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
(setq curvecen (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
(setq remainlist (member curvecen cgpointlist))
(setq curvecenc (cadr remainlist))
))
							   (linereader)
							   (if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
(setq cp2 (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
(setq remainlist (member cp2 cgpointlist))
(setq lp2c (cadr remainlist))
))
							   (linereader)

		(setq ,pos1 (vl-string-position 44 lp1c 0))
                 (setq east (atof (substr lp1c 1 ,pos1)))
      		 (setq north (atof (substr lp1c (+ ,pos1 2) 50)))
					   (setq tp1 (list east north))
                                           (setq p1 (list east north))
  (setq ,pos1 (vl-string-position 44 lp2c 0))
                 (setq east (atof (substr lp2c 1 ,pos1)))
      		 (setq north (atof (substr lp2c (+ ,pos1 2) 50)))
					   (setq tp2 (list east north))
                                           (setq p2 (list east north))
	    (setq digchaz (angle p1 p2))

;calc arc internal angle
	      (SETQ MAST (SQRT (- (* (atof RADIUS) (atof RADIUS)) (* (/ (distance p1 p2) 2)(/ (distance p1 p2) 2 )))))
  (SETQ O (* 2 (ATAN (/ (/ (distance p1 p2) 2) MAST))))
	    (setq remhalfO (/ (- pi O) 2))
	    ;calc bearing from p1 to arc centre
	    (if (= curverot "ccw")(setq raybearing (+  digchaz  remhalfO))(setq raybearing (-  digchaz  remhalfO)))
	    
	      ;CONVERT TO ANTI CLOCKWISE AND EAST ANGLE
  ;(SETQ raybearing (+ (* -1 raybearing) (* 0.5 PI)))

	       ;calc curve centre point
	    (setq curvecen (polar p1 raybearing (atof radius)))
	    (setq curvecenc (strcat (rtos (car curvecen) 2 9) "," (rtos (cadr curvecen) 2 9)))
							  

							 (if (= curverot "ccw") (command "arc" "c" curvecenc lp1c lp2c)(command "arc" "c" curvecenc lp2c lp1c))
							 (SETQ RMB (ENTLAST))
							  (SSADD RMB ENTSS)
							  (setq geolength (+ geolength 1))
							 ));p and if curve

	

					    (linereader);read line curve or end of coordinate geometery



       	));p and while </CoordGeom>					  

								  (setq ptlist (append ptlist (list lp2c)))

							  ;create polyline if plan feature is more than one object
   
							  (if (or (> geolength 1)(= (substr pclname 1 4) "CHAI"))(command "pedit" "m" entss "" "y" "j" "" ""))
							  ;if object is offset draw offset informaton

							  (if  (or (= (substr pclname 1 4) "Off-")(= (substr pclname 1 4) "OFF-"))(progn
										    (setq mp (list (/ (+ (car p1)(car p2)) 2)(/ (+ (cadr p1)(cadr p2))2 )))
										    (setq ang (angle p1 p2))
										    (if (and (> ang  (* 0.5 pi))(< ang (* 1.5 pi)))(setq ang (- ang pi)))
  (if (< ang 0)(setq ang (+ ang (* 2 pi))))
										    (setq off (atof (substr pcldesc 2 500)))
										    (if (> off (* th 5))(setq tpos mp
													      just "BC"))

					  (if (and (< off (* th 7))(>= (angle p1 p2) (* 0.5 pi))(<= (angle p1 p2)(* 1.5 pi)))(setq tpos p2
																	 just "BR"))
					  (if (and (< off (* th 7))(or(<= (angle p1 p2) (* 0.5 pi))(>= (angle p1 p2)(* 1.5 pi))))(setq tpos p2
																	 just "BL"))

										    

										    (SETQ BDINFO (STRCAT "desc=\"" pcldesc "\">"))
 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
	(setvar "clayer" "Drafting AFR")
	(COMMAND "TEXT" "J" JUST TPOS TH (ANGTOS ANG 1 4) (STRCASE pcldesc))
										    
	);if offset
							    (progn ;else



							      (if (= (substr pclname 1 4) "CHAI")(setq pcldesc  chainlist));make chainages to desc

							       (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 pcldesc)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
							      

          (if (AND (/= pcldesc "Walls")
	  (/= pcldesc "Building Return")
	  (/= pcldesc "Timber Walls")
	  (/= pcldesc "Fences")
   	  (/= pcldesc "Kerbs")
	  (/= pcldesc "Centreline")
	  (/= pcldesc "Railway")
 	  (/= pcldesc "Rockwall")
	  (/= pcldesc "Hedge")
	  (/= pcldesc "Other")
          (/= pcldesc "Gate")
	  (/= pcldesc "Not Fenced")
	  (/= (substr pcldesc 1 2) "00")
	  )

   
   (progn ;only label if detailed description
							     

			

	
		 (setq mp1 (nth (- (/ (length ptlist) 2)1) ptlist))
		 (setq mp2 (nth  (/ (length ptlist) 2) ptlist))
 
		(setq ,pos1 (vl-string-position 44 mp1 0))
                 (setq east (atof (substr mp1 1 ,pos1)))
      		 (setq north (atof (substr mp1 (+ ,pos1 2) 50)))
					   (setq mp1 (list east north))
 
		(setq ,pos1 (vl-string-position 44 mp2 0))
                 (setq east (atof (substr mp2 1 ,pos1)))
      		 (setq north (atof (substr mp2 (+ ,pos1 2) 50)))
					   (setq mp2 (list east north))
 
		 (setq mp (list (/ (+ (car mp1)(car mP2)) 2) (/ (+ (cadr mp1)(cadr mp2)) 2)))
		 (setq mprot (angle mp1 mp2))
		 (setq mprot90 (+ mprot (* 0.5 pi)))
  		 (SETQ 1POS (POLAR mp mprot90 (* TH 2.5)))
                 (IF (AND (> mprot  (* 0.5 pi)) (< mprot (* 1.5 pi)))(setq mprot (+ mprot pi))) ;if text is upsidedown reverse rotation



											      
                 (setvar "clayer" "Drafting AFR")
		 (COMMAND "TEXT" "J" "MC" 1pos TH (ANGTOS mprot 1 4) (strcase pcldesc))
		 (setvar "clayer" prevlayer)
		 ))

 );else


							    );p&if is an offset
       
            
       				
					    (linereader);read </PlanFeature>
      
					    (linereader);read next feature or end of features
       

							       
       
   ));p and while not </PlanFeatures>
	
));if plan features exist
  
;---------------------------------------------------------End if PLAN FEATURES

;--------------------------------------------------------SURVEY HEADER---------------------------------------
(princ "\nReading Survey header")

  ;shift note position down slightly after oc defintions
  (setq ocp (list (car ocp)(- (cadr ocp) 2)))

  
   (close xmlfile)
   (setq xmlfile (open xmlfilen "r"))
(linereader)

  ;linefeed to planfeatures
 (while (= (vl-string-search "<Survey" linetext) nil) ( progn
 (linereader)
))
 
;Else -ie no plan feautres and is survey
  ;(progn
  
  (linereader);read (Survey Header
  

    (if (/= (setq stringpos (vl-string-search "name" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq shname (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq shname nil))
   (if (/= (setq stringpos (vl-string-search "jurisdiction" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 14)))(setq shjur (substr linetext (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq shjur nil))
   (if (/= (setq stringpos (vl-string-search "desc" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq shdesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq shdesc nil))
   (if (/= (setq stringpos (vl-string-search "surveyorFirm" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 14)))(setq shfirm (substr linetext (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq shfirm nil))
   (if (/= (setq stringpos (vl-string-search "surveyorReference" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 19)))(setq shref (substr linetext (+ stringpos 20) (-(- wwpos 1)(+ stringpos 18)))))(setq shref nil))
  (if (/= (setq stringpos (vl-string-search "surveyFormat" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 14)))(setq shformat (substr linetext (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq shformat nil))
  (if (/= (setq stringpos (vl-string-search "type" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq shtype (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq shtype nil))

  (if (= shtype "compiled")(setq shtype "partially surveyed"))
  (if (= shtype "computed")(setq shtype "compiled"))

  (linereader);read first header line
  (setq shpurpose nil)
  (setq shdos nil
	shpu nil
	shpn nil
	shsurveyor nil
	shpurpose nil
	shlocality nil
	shlga nil
	shparish nil
	shcounty nil
	shsurveyregion nil
	shterrain nil
	subnum nil

	;VIC
	shts nil
	shcs nil
	shca nil
	shcp nil
	shhop nil
	 
	);reset all variable variables
	 
  (while (= (vl-string-search "</SurveyHeader>" linetext ) nil)( progn
;dates-----------------------------------
	  (if (and (/= (vl-string-search "<AdministrativeDate" linetext ) nil)(/= (vl-string-search "adminDateType=\"Date of Survey\"" linetext ) nil))
	    (progn
	    (setq stringpos (vl-string-search "adminDate=" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 11)))
	    (setq shdos (substr linetext (+ stringpos 12) (-(- wwpos 1)(+ stringpos 10))))
	    ))
	 
	  ;personel-------------------------------
	   (if (/= (vl-string-search "<Personnel" linetext ) nil)
	    (progn
	    (setq stringpos (vl-string-search "name" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
	    (setq shsurveyor (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))

	    ;VIC EXTRAS
	    (setq stringpos (vl-string-search "regType" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
	    (setq shregtype (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))))
	    	    (setq stringpos (vl-string-search "regNumber" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 11)))
	    (setq shregnumber (substr linetext (+ stringpos 12) (-(- wwpos 1)(+ stringpos 10))))
	    
	    ))
	  ;Annotation------------@@@@@other annotation types need to be added later

	  (if (/= (vl-string-search "<Annotation" linetext ) nil)(progn
	(setq stringpos (vl-string-search "type" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
	    (setq annotype (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
				(setvar "clayer" "Admin Sheet")
	
(setq &pos 0)
  (while (/=  (setq &pos (vl-string-search "&amp;" linetext &pos )) nil) (setq linetext (vl-string-subst "&" "&amp;"   linetext &pos)
										      &pos (+ &pos 1)))


;VIC
	  (if (= annotype "Township"  )
  	  	    (progn
	    (setq stringpos (vl-string-search "desc" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
	    (setq shts (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
	    ))
	(if (= annotype "Crown Section"  )
  	  	    (progn
	    (setq stringpos (vl-string-search "desc" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
	    (setq shcs (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
	    ))
	  (if (= annotype "Crown Allotment"  )
	      (progn
	    (setq stringpos (vl-string-search "desc" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
	    (setq shca (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
	    ))
	    (if (= annotype "Crown Portion"  )
	      (progn
	    (setq stringpos (vl-string-search "desc" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
	    (setq shcp (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
	    ))


	;normal notes
	  (if (member annotype notetypelist)(progn
					       (setq stringpos (vl-string-search "desc" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
	    (setq pcldesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
					      
					              (setq &pos 0)
					      	      (while (/=  (setq &pos (vl-string-search "&amp;" pcldesc &pos )) nil) (setq pcldesc (vl-string-subst "&" "&amp;"   pcldesc &pos)
										      &pos (+ &pos 1)))
				        (setq \pos 0)
					      	      (while (/=  (setq \pos (vl-string-search ( chr 92) pcldesc \pos )) nil) (setq pcldesc (vl-string-subst (strcat (chr 92) (chr 92)) (chr 92)   pcldesc \pos)
										      \pos (+ \pos 2)))

					      (setq crlfpos 0)
					      	      (while (/=  (setq crlfpos (vl-string-search "&#xD;&#xA;" pcldesc crlfpos )) nil) (setq pcldesc (vl-string-subst (chr 10) "&#xD;&#xA;"   pcldesc crlfpos)
										      crlfpos (+ crlfpos 1)))

				        (setq crlfpos 0)
					      	      (while (/=  (setq crlfpos (vl-string-search "&#xA;" pcldesc crlfpos )) nil) (setq pcldesc (vl-string-subst (chr 10) "&#xA;"   pcldesc crlfpos)
										      crlfpos (+ crlfpos 1)))
					          					
	    (setq quotpos 0)
	(while (/=  (setq quotpos (vl-string-search "&quot;" pcldesc quotpos )) nil) (setq pcldesc (vl-string-subst "\"" "&quot;"  pcldesc 	quotpos)
										      quotpos (+ quotpos 1)))
					      (setq apos 0)
	(while (/=  (setq apos (vl-string-search "&apos;" pcldesc apos )) nil) (setq pcldesc (vl-string-subst "'" "&apos;"  pcldesc apos)
										      apos (+ apos 1)))
	    (setq apos 0)
	    	(while (/=  (setq apos (vl-string-search "&#176;" pcldesc apos )) nil) (setq pcldesc (vl-string-subst "°" "&#176;"  pcldesc apos)
										      apos (+ apos 1)))
					      
					      
					      (if (/= (substr annotype 1 10) "Section 12")(progn
											    ( command "text" ocp "3.5" "90" annotype "")
											    (setq ocp (list (car ocp)(- (cadr ocp) 2)))
						));if not section 12(2) notation list title
										   
										(command "mtext" ocp "h" "2.5" "@132,-10" pcldesc "")
										 (SETQ LTINFO2 (strcat "<Annotation type=\"" annotype "\"") )
						    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO2)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)
										(setq ocp (list (car ocp) (- (cadr ocp) (+ 10(* 4  (/ (strlen pcldesc) 100))))))	      
));p&if normal note

	

  (if (member annotype noteandpcllist)(progn
					       (setq stringpos (vl-string-search "desc" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
	    (setq pcldesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))

				         (setq &pos 0)
					      	      (while (/=  (setq &pos (vl-string-search "&amp;" pcldesc &pos )) nil) (setq pcldesc (vl-string-subst "&" "&amp;"   pcldesc &pos)
										      &pos (+ &pos 1)))
				        (setq \pos 0)
					      	      (while (/=  (setq \pos (vl-string-search ( chr 92) pcldesc \pos )) nil) (setq pcldesc (vl-string-subst (strcat (chr 92) (chr 92)) (chr 92)   pcldesc \pos)
										      \pos (+ \pos 2)))

					(setq crlfpos 0)
					      	      (while (/=  (setq crlfpos (vl-string-search "&#xD;&#xA;" pcldesc crlfpos )) nil) (setq pcldesc (vl-string-subst (chr 10) "&#xD;&#xA;"   pcldesc crlfpos)
										      crlfpos (+ crlfpos 1)))

				        (setq crlfpos 0)
					      	      (while (/=  (setq crlfpos (vl-string-search "&#xA;" pcldesc crlfpos )) nil) (setq pcldesc (vl-string-subst (chr 10) "&#xA;"   pcldesc crlfpos)
										      crlfpos (+ crlfpos 1)))
				       					
(setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" pcldesc &pos )) nil) (setq pcldesc (vl-string-subst "&" "&amp;"  pcldesc &pos)
										      &pos (+ &pos 1)))
	    (setq quotpos 0)
	(while (/=  (setq quotpos (vl-string-search "&quot;" pcldesc quotpos )) nil) (setq pcldesc (vl-string-subst "\"" "&quot;"  pcldesc 	quotpos)
										      quotpos (+ quotpos 1)))
					      (setq apos 0)
	(while (/=  (setq apos (vl-string-search "&apos;" pcldesc apos )) nil) (setq pcldesc (vl-string-subst "'" "&apos;"  pcldesc apos)
										      apos (+ apos 1)))
	    (setq apos 0)
	    	(while (/=  (setq apos (vl-string-search "&#176;" pcldesc apos )) nil) (setq pcldesc (vl-string-subst "°" "&#176;"  pcldesc apos)
										      apos (+ apos 1)))
			
					
					
					(setq stringpos (vl-string-search "pclRef" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
	    (setq pclref (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
					
					       (if (/= annotype "Parcel with Area by Deduction")(progn;if not area by deduciton
					      					    ( command "text" ocp "3.5" "90" ( strcat pclref "-" annotype  ""))
											    (setq ocp (list (car ocp)(- (cadr ocp) 2)))
						)
						 
						; (setq pcldesc (strcat pcldesc " - " pclref));if area by deduction
						 
						 					 
						 );if not area by deduction

						 (setq \pos 0)
					      	      (while (/=  (setq \pos (vl-string-search ( chr 92) pcldesc \pos )) nil) (setq pcldesc (vl-string-subst (strcat (chr 92) (chr 92)) (chr 92)   pcldesc \pos)
										      \pos (+ \pos 2)))
						 										   
										(command "mtext" ocp "h" "2.5" "@132,-10" pcldesc "")
										 (SETQ LTINFO2 (strcat "<Annotation type=\"" annotype "\" pclRef=\"" pclref "\"" ) )
						    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO2)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)
						 
						   
										(setq ocp (list (car ocp) (- (cadr ocp) (+ 10(* 4  (/ (strlen pcldesc) 100))))))	      
	));if parcel note

		

  (if (member annotype nandoppcllist)(progn
					       (setq stringpos (vl-string-search "desc" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
	    (setq pcldesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))

				              (setq &pos 0)
					      	      (while (/=  (setq &pos (vl-string-search "&amp;" pcldesc &pos )) nil) (setq pcldesc (vl-string-subst "&" "&amp;"   pcldesc &pos)
										      &pos (+ &pos 1)))
				        (setq \pos 0)
					      	      (while (/=  (setq \pos (vl-string-search ( chr 92) pcldesc \pos )) nil) (setq pcldesc (vl-string-subst (strcat (chr 92) (chr 92)) (chr 92)   pcldesc \pos)
										      \pos (+ \pos 2)))
				       (setq crlfpos 0)
					      	      (while (/=  (setq crlfpos (vl-string-search "&#xD;&#xA;" pcldesc crlfpos )) nil) (setq pcldesc (vl-string-subst (chr 10) "&#xD;&#xA;"   pcldesc crlfpos)
										      crlfpos (+ crlfpos 1)))

				        (setq crlfpos 0)
					      	      (while (/=  (setq crlfpos (vl-string-search "&#xA;" pcldesc crlfpos )) nil) (setq pcldesc (vl-string-subst (chr 10) "&#xA;"   pcldesc crlfpos)
										      crlfpos (+ crlfpos 1)))
				       					
(setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" pcldesc &pos )) nil) (setq pcldesc (vl-string-subst "&" "&amp;"  pcldesc &pos)
										      &pos (+ &pos 1)))
	    (setq quotpos 0)
	(while (/=  (setq quotpos (vl-string-search "&quot;" pcldesc quotpos )) nil) (setq pcldesc (vl-string-subst "\"" "&quot;"  pcldesc 	quotpos)
										      quotpos (+ quotpos 1)))
					      (setq apos 0)
	(while (/=  (setq apos (vl-string-search "&apos;" pcldesc apos )) nil) (setq pcldesc (vl-string-subst "'" "&apos;"  pcldesc apos)
										      apos (+ apos 1)))
	    (setq apos 0)
	    	(while (/=  (setq apos (vl-string-search "&#176;" pcldesc apos )) nil) (setq pcldesc (vl-string-subst "°" "&#176;"  pcldesc apos)
										      apos (+ apos 1)))
				       
				       
					(if (/= (setq stringpos (vl-string-search "pclRef" linetext )) nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
	    (setq pclref (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7)))))(setq pclref ""))

				       (if (/= pclref "")
					 (setq minus "-"
					       pclrefs (strcat " pclRef\"" pclref "\""))
					 (setq minus ""
					       pclrefs ""))
					       
					      					    ( command "text" ocp "3.5" "90" ( strcat pclref minus annotype  ""))
											    (setq ocp (list (car ocp)(- (cadr ocp) 2)))
												
						 										   
										(command "mtext" ocp "h" "2.5" "@132,-10" pcldesc "")
				       
										 (SETQ LTINFO2 (strcat "<Annotation type=\"" annotype "\""  pclrefs ))
						    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO2)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)
						 
						   
										(setq ocp (list (car ocp) (- (cadr ocp) (+ 10(* 4  (/ (strlen pcldesc) 100))))))	      
	));if optional parcel note


  (if (= annotype "Surveyor's Report Notation")(progn
					       (setq stringpos (vl-string-search "desc" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
	    (setq pcldesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))

				              (setq &pos 0)
					      	      (while (/=  (setq &pos (vl-string-search "&amp;" pcldesc &pos )) nil) (setq pcldesc (vl-string-subst "&" "&amp;"   pcldesc &pos)
										      &pos (+ &pos 1)))
				        (setq \pos 0)
					      	      (while (/=  (setq \pos (vl-string-search ( chr 92) pcldesc \pos )) nil) (setq pcldesc (vl-string-subst (strcat (chr 92) (chr 92)) (chr 92)   pcldesc \pos)
										      \pos (+ \pos 2)))
					       (setq crlfpos 0)
					      	      (while (/=  (setq crlfpos (vl-string-search "&#xD;&#xA;" pcldesc crlfpos )) nil) (setq pcldesc (vl-string-subst (chr 10) "&#xD;&#xA;"   pcldesc crlfpos)
										      crlfpos (+ crlfpos 1)))

				        (setq crlfpos 0)
					      	      (while (/=  (setq crlfpos (vl-string-search "&#xA;" pcldesc crlfpos )) nil) (setq pcldesc (vl-string-subst (chr 10) "&#xA;"   pcldesc crlfpos)
										      crlfpos (+ crlfpos 1)))
				       					
(setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" pcldesc &pos )) nil) (setq pcldesc (vl-string-subst "&" "&amp;"  pcldesc &pos)
										      &pos (+ &pos 1)))
	    (setq quotpos 0)
	(while (/=  (setq quotpos (vl-string-search "&quot;" pcldesc quotpos )) nil) (setq pcldesc (vl-string-subst "\"" "&quot;"  pcldesc 	quotpos)
										      quotpos (+ quotpos 1)))
					      (setq apos 0)
	(while (/=  (setq apos (vl-string-search "&apos;" pcldesc apos )) nil) (setq pcldesc (vl-string-subst "'" "&apos;"  pcldesc apos)
										      apos (+ apos 1)))
	    (setq apos 0)
	    	(while (/=  (setq apos (vl-string-search "&#176;" pcldesc apos )) nil) (setq pcldesc (vl-string-subst "°" "&#176;"  pcldesc apos)
										      apos (+ apos 1)))
				       
				       
					(if (/= (setq stringpos (vl-string-search "pclRef" linetext )) nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
	    (setq pclref (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7)))))(setq pclref ""))

				       (if (/= pclref "")
					 (setq minus "-"
					       pclrefs (strcat " pclRef\"" pclref "\""))
					 (setq minus ""
					       pclrefs ""))
					       
					      					    ( command "text" srp "3.5" "90" ( strcat pclref minus annotype  ""))
											    (setq srp (list (car srp)(- (cadr srp) 2)))
												
						 										   
										(command "mtext" srp "h" "2.5" "@250,-10" pcldesc "")
				       
										 (SETQ LTINFO2 (strcat "<Annotation type=\"" annotype "\""  pclrefs ))
						    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO2)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)
						 
						   
											      
	));if surveyors report parcel note


	


	

	(if (= annotype "Easement Width")(progn

					    (setq stringpos (vl-string-search "desc" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
	    (setq pcldesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
					   
					   (setq stringpos (vl-string-search "pclRef" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
	    (setq pclref (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))

    (IF (/= (setq lots (ssget "_X" '((0 . "POINT") (8 . "Lot Definitions")))) nil)(progn

	(setq count 0)
  (repeat (sslength lots)
     (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))
    (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME lots COUNT)))))
      	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Link parcel with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
        
    
 ;seperate out link parcel
     

    (if (/= (setq !pos1 (vl-string-position 33 xdatai 0)) nil)  (setq easelink (substr xdatai (+ !pos1 2) 200))(setq easelink ""));look for !
                      

		     
    (PRINC (STRCAT "\n" easelink "-" pclref))
(if (= easelink pclref)(setq ep (list (+ (car p1) 95)(cadr p1))))
   
    
    (setq count (+ count 1))
	  )
	))

    	(command "mtext" ep "h" "2.5" "@18,2.5" pcldesc "")
	 (SETQ LTINFO2 (strcat "<Annotation type=\"" annotype "\" pclRef=\"" pclref "\"" ))
    (SETQ SENT (ENTLAST))
						    (SETQ SENTLIST (ENTGET SENT))
						    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 LTINFO2)))))
						    (setq NEWSENTLIST (APPEND SENTLIST XDATA))
						    (ENTMOD NEWSENTLIST)

    ));if easement width



));p&if annotation

	  
	   (if (/= (vl-string-search "<HeadOfPower" linetext ) nil)
	    (progn
	    (setq stringpos (vl-string-search "name" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
	    (if (/= shhop nil)(setq shhop (strcat shhop " & " (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))
	    (setq shhop (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))
	    ))
	  


	 

	  
	  ;Purpose-------------------- note mulitples are joined together with commas
	  (if (/= (vl-string-search "<PurposeOfSurvey" linetext ) nil)
	    (progn
	      
	    (setq stringpos (vl-string-search "name" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
	    (if (= shpurpose nil)
	    (setq shpurpose (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
	     (setq shpurpose (strcat  (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))"," shpurpose))
	      )
	    ;if normal purpose
	    (if (/= (setq -pos1 (vl-string-position 45 shpurpose 0)) nil)(progn
                 (setq shpurpose1  (substr shpurpose 1 -pos1))
      		 (setq shpurpose2  (substr shpurpose (+ -pos1 2) 2000))
		 ));if normal purpose
	    ;if non descriptive purpose 26 or 37(8)
	    (if (= shpurpose "Section 26")(setq shpurpose1 shpurpose
						shpurpose2 "Boundary Plan"))
	    (if (= shpurpose "Section 37(8)")(setq shpurpose1 shpurpose
						shpurpose2 "Plan of Subdivision (Staged Plan)"))
	    
	    	    ))
	  
	  		      

	  
	  ;Administrative area
	  (if (and (/= (vl-string-search "<AdministrativeArea" linetext ) nil)(/= (vl-string-search "adminAreaType=\"Locality\"" linetext ) nil))
	    (progn
	    (setq stringpos (vl-string-search "adminAreaName" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
	    (setq shlocality (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14))))
	    ))
	  (if (and (/= (vl-string-search "<AdministrativeArea" linetext ) nil)(/= (vl-string-search "adminAreaType=\"LGA\"" linetext ) nil))
	    (progn
	    (setq stringpos (vl-string-search "adminAreaName" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
	    (setq shlga (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14))))
	     (setq stringpos (vl-string-search "adminAreaCode" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
	    (setq shlgacode (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14))))
	    ))
	  (if (and (/= (vl-string-search "<AdministrativeArea" linetext ) nil)(/= (vl-string-search "adminAreaType=\"Parish\"" linetext ) nil))
	    (progn
	    (setq stringpos (vl-string-search "adminAreaName" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
	    (setq shparish (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14))))
	     (setq stringpos (vl-string-search "adminAreaCode" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
	    (setq shparishcode (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14))))
	    
	    ))
	  (if (and (/= (vl-string-search "<AdministrativeArea" linetext ) nil)(/= (vl-string-search "adminAreaType=\"County\"" linetext ) nil))
	    (progn
	    (setq stringpos (vl-string-search "adminAreaName" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
	    (setq shcounty (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14))))
	    ))
	  (if (and (/= (vl-string-search "<AdministrativeArea" linetext ) nil)(/= (vl-string-search "adminAreaType=\"Survey Region\"" linetext ) nil))
	    (progn
	    (setq stringpos (vl-string-search "adminAreaName" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
	    (setq shsurveyregion (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14))))
	    ))
	  	  (if (and (/= (vl-string-search "<AdministrativeArea" linetext ) nil)(/= (vl-string-search "adminAreaType=\"Terrain\"" linetext ) nil))
	    (progn
	    (setq stringpos (vl-string-search "adminAreaName" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
	    (setq shterrain (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14))))
	    ))

	   (linereader)
	  
	  					    
  ));p & while not </surveyheader>

  ;check results
  (if (= shname nil)(setq shname ""))
  (if (= shlga nil)(setq shlga ""))
  (if (= shlocality nil)(setq shlocality ""))
  (if (= shparish nil)(setq shparish ""))
  (if (= shcounty nil)(setq shcounty ""))
  (if (= shsurveyor nil)(setq shsurveyor ""))
  (if (= shfirm nil)(setq shfirm ""))
  (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" shfirm &pos )) nil) (setq shfirm (vl-string-subst "&" "&amp;"  shfirm &pos)
										      &pos (+ &pos 1)))
   (setq crlfpos 0)
	   (while (/=  (setq crlfpos (vl-string-search "&#xA;" shfirm crlfpos )) nil) (setq shfirm (vl-string-subst "\P" "&#xA;"  shfirm crlfpos)
										      crlfpos (+ crlfpos 2)))				 
  (if (= shdos nil)(setq shdos ""))
  (if (= shsurveyregion nil)(setq shsurveyregion ""))
  (if (= shterrain nil)(setq shterrain ""))
  (if (= shref nil)(setq shref ""))
  (if (= shdesc nil)(setq shdesc "none"))
    (setq &pos 0)
  	      (while (/=  (setq &pos (vl-string-search "&amp;" shdesc &pos )) nil) (setq shdesc (vl-string-subst "&" "&amp;"  shdesc &pos)
   										      &pos (+ &pos 1)))
  (setq crlfpos 0)
	   (while (/=  (setq crlfpos (vl-string-search "&#xA;" shdesc crlfpos )) nil) (setq shdesc (vl-string-subst "\P" "&#xA;"  shdesc crlfpos)
										      crlfpos (+ crlfpos 2)))	
  (if (= shpu nil)(setq shpu "none"))
  (setq crlfpos 0)
	   (while (/=  (setq crlfpos (vl-string-search "&#xA;" shpu crlfpos )) nil) (setq shpu (vl-string-subst "\\P" "&#xA;"  shpu crlfpos)
										      crlfpos (+ crlfpos 2)))	
    (if (= shpurpose nil)(setq shpurpose ""))
  (if (= shdor nil)(setq shdor ""))
  (if (= shtype nil)(setq shtype ""))
  (if (= shjur nil)(setq shjur ""))
  (if (= shformat nil)(setq shformat ""))
  (if (= shformat "Standard" )(setq shname (strcat "DP" shname)))
  (if (= shformat "Stratum" )(setq shname (strcat "DP" shname)))
  (if (= shformat "Strata")(setq shname (strcat "SP" shname )))
  (if (= shpln nil)(setq shpln "none"))

  ;vic
  (if (= shts nil)(setq shts ""))
  (if (= shca nil)(setq shca ""))
  (if (= shcs nil)(setq shcs ""))
  (if (= shcp nil)(setq shcp ""))
  (if (= shpurpose1 nil)(setq shpurpose1 ""))
  (if (= shpurpose2 nil)(setq shpurpose2 ""))
  (if (= shhop nil)(setq shhop ""))
  (if (= shregnumber nil)(setq shregnumber ""))
    (if (= shlgacode nil)(setq shlgacode ""))
  (if (= shparishcode nil)(setq shparishcode ""))

  (setq &pos 0)
  	      (while (/=  (setq &pos (vl-string-search "&amp;" shpln &pos )) nil) (setq shpln (vl-string-subst "&" "&amp;"  shpln &pos)
   										      &pos (+ &pos 1)))
  (setq crlfpos 0)
	   (while (/=  (setq crlfpos (vl-string-search "&#xA;" shpln crlfpos )) nil) (setq shpln (vl-string-subst "\\P" "&#xA;"  shpln crlfpos)
										      crlfpos (+ crlfpos 2)))	
  
  
  (if (= subnum nil)(setq subnum ""))
  ;@@@@@date of image and lodgement not used, couldnt find it on the exisiting plans.

  (setq tbinsertpoint (list (+ maxeast 10) minnorth))

    (if (/= shdor "")(progn  
	    ;rearrage shdor to dd.mm.yyyy format for text output
  (setq minuspos1 (vl-string-position 45 shdor 0))
  (setq minuspos2 (vl-string-position 45 shdor (+ minuspos1 1)))
  (if  (= minuspos1 4)(progn;rearrage date to year last
				       (setq year  (substr shdor 1 minuspos1))
				       (if (= (strlen year) 1) (setq year (strcat "0" year)));single digit days
				       (setq month (substr shdor (+ minuspos1 2) (- (- minuspos2 minuspos1) 1)))
				       (if (= (strlen month) 1) (setq month (strcat "0" month)));single digit days
				       (setq day  (substr shdor (+ minuspos2 2) 50))
				       (setq shdor (strcat day "-" month "-" year))
				       ));p&if
  ));if dor not ""

  
  	    ;rearrage shdos to dd.mm.yyyy format for text output
  (if (/= shdos "")(progn
  (setq minuspos1 (vl-string-position 45 shdos 0))
  (setq minuspos2 (vl-string-position 45 shdos (+ minuspos1 1)))
  (if  (= minuspos1 4)(progn;rearrage date to year last
				       (setq year  (substr shdos 1 minuspos1))
				       (if (= (strlen year) 1) (setq year (strcat "0" year)));single digit days
				       (setq month (substr shdos (+ minuspos1 2) (- (- minuspos2 minuspos1) 1)))
				       (if (= (strlen month) 1) (setq month (strcat "0" month)));single digit days
				       (setq day  (substr shdos (+ minuspos2 2) 50))
				       (setq shdos (strcat day "-" month "-" year))
				       ));p&if
  ));if dos not ""
(setq exttitles "")
(setq extaddress "")
  (setq shdl "")
  
  (if (>= (length exttlist) 1)(progn
			       
			       
			       (setq exttitles (nth 0 exttlist))
			       (setq exttcount 1)
			       (repeat (- (length exttlist) 1)
				 (setq exttitles (strcat exttitles "," (nth exttcount exttlist) ))
				 (setq exttcount (+ exttcount 1))
				 )
			       ))
 
     (if (>= (length extalist) 1)(progn
			       
			       (setq extaddress (nth 0 extalist))
			       (setq extacount 1)
			       (repeat (- (length extalist) 1)
				 (setq extaddress (strcat   extaddress (chr 10)  (nth extacount extalist)))
				 (setq extacount (+ extacount 1))
				 )
			       ))
  
     (if (>= (length dllist) 1)(progn
			      
			        (setq shdl  (nth 0 dllist))
			       (setq dlcount 1)
			       (repeat (- (length dllist) 1)
				 (setq shdl (strcat shdl "," (nth dlcount dllist)))
				   (setq dlcount (+ dlcount 1))
				 )
			       ))
  

    (IF (= exttitles "")(setq exttitles " "))
  (if (= extaddress "")(setq extaddress " "))
    
		(setq shlpr "");last plan reference not stored in xml
  
  (SETVAR "CLAYER"  "Admin Sheet" )
  ;insert titleblock
;changed for BricsCAD
  ;(princ (strcat  shname shlga shparish shfirm shref shpurpose1 shca shcs exttitles "" extaddress "" shlpr shdl shtype shpurpose2 shhop shsurveyor shregnumber shdos shjur shformat datum hdatum zone shlgacode shparishcode))
  (COMMAND "._INSERT" "ADMINSHEET" "_S" "1" tbinsertpoint "0" shname shlga shparish shfirm shref  shts shcs shca shcp exttitles "" extaddress "" shlpr shdl shtype shpurpose2 shhop shsurveyor shregnumber shdos shjur shformat datum hdatum zone shlgacode shparishcode shpurpose1)
   
  ;instrument stations
;  (setq linetext (read-line xmlfile))
;(while (= (vl-string-search "<ObservationGroup" linetext ) nil)( progn
;		   (if (/= (setq stringpos (vl-string-search "id" linetext )) nil)(progn
;(setq wwpos (vl-string-position 34 linetext (+ stringpos 4)))(setq isid (substr linetext (+ stringpos 5) (-(- wwpos 1)(+ stringpos 3)))))(setq isid nil))
		   ;(setq linetext (read-line xmlfile))
		   ;(if (/= (setq stringpos (vl-string-search "pntRef" linetext )) nil)(progn
;(setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))(setq iscgp (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7)))))(setq iscgp nil))
;		   (setq isid (strcat "IS-" isid));add "is-" to isid just in case someone is stupid enough to use the same id
;		   (setq islist (append islist (list isid)(list iscgp)))
;
;		   (setq linetext (read-line xmlfile));read end of is
;		   (setq linetext (read-line xmlfile));read next line
		   
;));p&w not observation group



  ;OBERSVATIONS


(princ "\nReading Reading Reduced Observations")

  ;CLOSE FILE AND READ TO OBSERVATION GROUP

  (close xmlfile)
   (setq xmlfile (open xmlfilen "r"))

   (while (= (vl-string-search "<ObservationGroup" linetext) nil) ( progn
 (linereader)
))


  

  (linereader)
  
(while (= (vl-string-search "</ObservationGroup>" linetext ) nil)( progn

								   (setq rmline 0);reset trigger for rmline with monument at other end.

		;line observation--------------------------------------------------------------------------------------------------
		(if (/= (vl-string-search "<ReducedObservation" linetext ) nil)
	    (progn
	      
	      
	    (if (/= (setq stringpos (vl-string-search "desc" linetext )) nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
            (setq desc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
	    )(setq desc ""))

	    (setq stringpos (vl-string-search "setupID" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
            (setq setupids (strcat "IS-" (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8)))))
	    (setq remlist (member setupids islist))
	    (setq setupid (cadr remlist))

	    (setq stringpos (vl-string-search "targetSetupID" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
            (setq targetid (strcat "IS-" (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))
	    (setq remlist (member targetid islist))
	    (setq targetid (cadr remlist))

	    (setq stringpos (vl-string-search "azimuth" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
            (setq bearing (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))))
	    (setq xbearing bearing)

	     (setq stringpos (vl-string-search "purpose" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
            (setq purpose (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))))
	    (if (= purpose "normal" )(setq purpose "Boundary"))
	    (setq rolayer purpose)
	    

	    (if (/= (setq stringpos (vl-string-search "horizDistance" linetext )) nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
            (setq dist (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq dist ""))

	    (if (/= (setq stringpos (vl-string-search "distanceType=" linetext ))nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 14)))
            (setq distancetype (strcat " distanceType=\"" (substr linetext (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13))) "\" ")))(setq distancetype ""))

	    (if (/= (setq stringpos (vl-string-search "azimuthType=" linetext ))nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 13)))
            (setq azimuthtype (strcat " azimuthType=\"" (substr linetext (+ stringpos 14) (-(- wwpos 1)(+ stringpos 12))) "\" ")))(setq azimuthtype ""))

	    (if (/= (setq stringpos (vl-string-search "distanceAdoptionFactor" linetext )) nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 24)))
            (setq daf (substr linetext (+ stringpos 25) (-(- wwpos 1)(+ stringpos 23))))
	    (if (/= daf "")(setq daf1 daf)))(setq daf ""))

	    (if (setq stringpos (vl-string-search "angleAccClass" linetext ))(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
            (setq sdb (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq sdb ""))

	    (if (setq stringpos (vl-string-search "distanceAccClass" linetext ))(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 18)))
            (setq sdd (substr linetext (+ stringpos 19) (-(- wwpos 1)(+ stringpos 17)))))(setq sdd ""))
		  

	    (if (/= (setq stringpos (vl-string-search "<FieldNote>" linetext )) nil)(progn
											   
(if (setq wwpos (vl-string-position 34 linetext (+ stringpos 12)))
  (progn;if field not contains ""s
    (setq comment (substr linetext (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11))))
    )
  (progn; else use the < to get other end of field note
    (setq <pos (vl-string-position 60 linetext (+ stringpos 11)))
    (setq comment (substr linetext (+ stringpos 12) (-(- <pos 1)(+ stringpos 10))))
    )
  )
)
  (setq comment ""))

(setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" comment &pos )) nil) (setq comment (vl-string-subst "&" "&amp;"  comment &pos)
										      &pos (+ &pos 1)))
	    
	    

	    (setq remainlist (member setupid cgpointlist))
            (setq lp1c (cadr remainlist))
	    (setq p1type (caddr remainlist))
	    (setq remainlist (member targetid cgpointlist))
            (setq lp2c (cadr remainlist))
	    (setq p2type (caddr remainlist))
	    ;add reverse of line to arclist
	    (setq linelist (append linelist (list (strcat lp2c","lp1c "," rolayer))))
	    (setq linedrawn nil)

	    (if (or (= p1type "n")(= p2type "n"))(setq rolayer "Irregular Right Lines"))
	    
	    (if (or (member (strcat lp1c "," lp2c) finaleaselist)(member (strcat lp2c "," lp1c) finaleaselist))(setq rolayer "Easement"));check if line is a easement line
 (if (or (member (strcat lp1c "," lp2c) extglist)(member (strcat lp2c "," lp1c) extglist))(setq rolayer "Boundary Extinguished"));check if line is a extinguished line

	       ;APPLY ALL CORRECTIONS AND EXTRACT INFORMATION FROM USER INPUT
(if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
  (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins   (substr bearing (+ dotpt1 2) 2) )
  (if (= (strlen mins) 1)(setq mins (strcat  mins "0")));fix problem with truncating zeros on minutes and seconds
  (setq mins (strcat mins "'"))
  (setq sec  (substr bearing (+ dotpt1 4) 10))
  (if (= (strlen sec) 1) (setq sec (strcat sec "0")))

  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))
  
  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

      
(setq bearing (strcat  deg "d" mins sec))
    ;(setq lbearing bearing)
	    (if (/= dist "")(setq dist (rtos (atof dist)2 3)));remove trailing zeros
	    
  (setq ldist (strcat dist ))
	    
    





	    ;    ;if nothing at either end of line
	    ;(if (= (or (member setupid pmlist)(member targetid pmlist)(member targetid rmlist)(member setupid rmlist)(= linedrawn "1")) nil)(progn

      (SETVAR "CLAYER"  rolayer )
      (if (member  (strcat lp1c ","lp2c "," rolayer) linelist)()
  (progn

    (if (or (= azimuthtype  "azimuthType=\"Estimated\" ") (= distancetype " distanceType=\"Estimated\" "))
      (progn
	(setvar "CECOLOR" "1")
	(princ "\nCAUTION - Plan contains estimated distance (marked red)")
	(setq comment (strcat comment " ESTIMATED"))
	))

    
      (command "line" lp1c lp2c "")
      
  (if (/= comment "")(setq ocomment(strcat "><FieldNote>\"" comment "\"</FieldNote></ReducedObservation>"))(setq ocomment "/>"))
    (if (/= desc "")(setq desc (strcat " desc=\"" desc "\"")))
    (if (/= sdb "")(setq sdb (strcat " angleAccClass=\"" sdb "\"")))
    (if (/= sdd "")(setq sdd (strcat " distanceAccClass=\"" sdd "\"")))
    (if (/= dist "")(setq disto (strcat " horizDistance=\"" dist "\""))(setq disto ""))
(SETQ BDINFO (STRCAT "azimuth=\"" xbearing "\"" disto sdb sdd azimuthtype distancetype  desc ocomment))
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

      (if (member (strcat lp1c "," lp2c "," rolayer) linelist)()(lba));label line if not already labelled
    ))

	    (if (or (= azimuthtype  "azimuthType=\"Estimated\" ") (= distancetype " distanceType=\"Estimated\" "))(setvar "CECOLOR" "BYLAYER"))

	    (if (= rolayer "traverse");add triangles if traverse line
	      (progn
		(if (= (member lp1c trilist) nil)(progn
						   (command "insert" "VSTN" "_S" th lp1c "0")
						   (setq trilist (append trilist (list lp1c)))))
	      (if (= (member lp2c trilist) nil)(progn
						   (command "insert" "VSTN" "_S" th lp2c "0")
						   (setq trilist (append trilist (list lp2c)))))
	      
	      
	      ));p&if traverse

	    ; );p
  ;  );if nothing at either end of line
      
  
	          (setq linelist (append linelist (list (strcat lp1c","lp2c "," rolayer))))
  

	    	    ));pif line


;------------arc observation-------------------------------------------------------------------------------------------------


		(if (/= (vl-string-search "<ReducedArcObservation" linetext ) nil)
	    (progn
	      
	      
	    (if (/= (setq stringpos (vl-string-search "desc" linetext )) nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
            (setq desc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq desc ""))

	    (setq stringpos (vl-string-search "purpose" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
            (setq purpose (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))))
	    (if (= purpose "normal" )(setq purpose "Boundary"))
            (setq rolayer purpose)

	    (setq stringpos (vl-string-search "setupID" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
            (setq setupids (strcat "IS-" (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8)))))
	    (setq remlist (member setupids islist))
	    (setq setupid (cadr remlist))

	    (setq stringpos (vl-string-search "targetSetupID" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
            (setq targetid (strcat "IS-" (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))
	    (setq remlist (member targetid islist))
	    (setq targetid (cadr remlist))

	    (setq stringpos (vl-string-search "chordAzimuth" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 14)))
            (setq bearing (substr linetext (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13))))
	    (setq xbearing bearing)

	    (setq stringpos (vl-string-search "length" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
            (setq arclength (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
	    (setq arclength (rtos (atof arclength)2 3));remove trailing zeros

	    (setq stringpos (vl-string-search "radius" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
            (setq radius (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
	    

	    (setq stringpos (vl-string-search "rot" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 5)))
            (setq curverot (substr linetext (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4))))

	    
	    (if (/= (setq stringpos (vl-string-search "arcType" linetext )) nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
            (setq arcType (strcat " arcType=\"" (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))) "\" ")))(setq arcType ""))

	    (if (/= (setq stringpos (vl-string-search "distanceType=" linetext ))nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 14)))
            (setq distancetype (strcat " distanceType=\"" (substr linetext (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13))) "\" ")))(setq distancetype ""))

	    (if (/= (setq stringpos (vl-string-search "azimuthType=" linetext ))nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 13)))
            (setq azimuthtype (strcat " azimuthType=\"" (substr linetext (+ stringpos 14) (-(- wwpos 1)(+ stringpos 12))) "\" ")))(setq azimuthtype ""))

	    (if (setq stringpos (vl-string-search "angleAccClass" linetext ))(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
            (setq sdb (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq sdb ""))

	    (if (setq stringpos (vl-string-search "distanceAccClass" linetext ))(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 18)))
            (setq sdd (substr linetext (+ stringpos 19) (-(- wwpos 1)(+ stringpos 17)))))(setq sdd ""))
		  

	       (if (/= (setq stringpos (vl-string-search "<FieldNote>" linetext )) nil)(progn
											   
(if (setq wwpos (vl-string-position 34 linetext (+ stringpos 12)))
  (progn;if field not contains ""s
    (setq comment (substr linetext (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11))))
    )
  (progn; else use the < to get other end of field note
    (setq <pos (vl-string-position 60 linetext (+ stringpos 11)))
    (setq comment (substr linetext (+ stringpos 12) (-(- <pos 1)(+ stringpos 10))))
    )
  )
)
  (setq comment ""))
	    (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" comment &pos )) nil) (setq comment (vl-string-subst "&" "&amp;"  comment &pos)
										      &pos (+ &pos 1)))


	    (setq remainlist (member setupid cgpointlist))
            (setq lp1c (cadr remainlist))
	    (setq remainlist (member targetid cgpointlist))
            (setq lp2c (cadr remainlist))


	    (if (or (member (strcat lp1c "," lp2c) finaleaselist)(member (strcat lp2c "," lp1c) finaleaselist))(setq rolayer "Easement")) ;check if line is an easement line
            (if (or (member (strcat lp1c "," lp2c) extglist)(member (strcat lp2c "," lp1c) extglist))(setq rolayer "Boundary Extinguished"));check if line is a extinguished line
  (setq ,pos1 (vl-string-position 44 lp1c 0))
                 (setq east (atof (substr lp1c 1 ,pos1)))
      		 (setq north (atof (substr lp1c (+ ,pos1 2) 50)))
					   (setq tp1 (list east north))
                                           (setq p1 (list east north))
  (setq ,pos1 (vl-string-position 44 lp2c 0))
                 (setq east (atof (substr lp2c 1 ,pos1)))
      		 (setq north (atof (substr lp2c (+ ,pos1 2) 50)))
					   (setq tp2 (list east north))
                                           (setq p2 (list east north))
	    (setq digchaz (angle p1 p2))

;calc arc internal angle
	      (SETQ MAST (SQRT (- (* (atof RADIUS) (atof RADIUS)) (* (/ (distance p1 p2) 2)(/ (distance p1 p2) 2 )))))
  (SETQ O (* 2 (ATAN (/ (/ (distance p1 p2) 2) MAST))))
	    (setq remhalfO  (- (* 0.5 pi) (/ O 2)))
	    ;calc bearing from p1 to arc centre (watching for bulbous arcs)
	    (if (and (= curverot "ccw") (<= (atof arclength) (* pi (atof radius))))(setq raybearing (+  digchaz  remhalfO)))
	    (IF (and (= curverot "cw") (<= (atof arclength) (* pi (atof radius))))(setq raybearing (-  digchaz  remhalfO)))
	    (IF (and (= curverot "ccw") (> (atof arclength) (* pi (atof radius))))(setq raybearing (-  digchaz  remhalfO)))
	    (if (and (= curverot "cw") (> (atof arclength) (* pi (atof radius))))(setq raybearing (+  digchaz  remhalfO)))

	    
	      ;CONVERT TO ANTI CLOCKWISE AND EAST ANGLE
  ;(SETQ raybearing (+ (* -1 raybearing) (* 0.5 PI)))

	       ;calc curve centre point
	    (setq curvecen (polar p1 raybearing (atof radius)))
	    (setq curvecenc (strcat (rtos (car curvecen) 2 9) "," (rtos (cadr curvecen) 2 9)))

	    ;calc curve midpoint
  (setq a1 (angle curvecen p1))
  (setq a2 (angle curvecen p2))
  (if (= curverot "ccw")(setq da (- a2 a1))(setq da (- a1 a2)))
  (if (< da 0)(setq da (+ da (* 2 pi))))
    (SETQ DA (/ DA 2))
    (IF (= CURVEROT "ccw")(setq midb (+ a1 da))(setq midb (+ a2 da)))
  (setq amp (polar curvecen midb (atof radius)))
	    
	    (setq arclist (append arclist (list (strcat lp2c"," curvecenc "," lp1c "," rolayer))))
	    
  					   
;calc chord distance, note using string values not digital values
	    (setq stringO (/ (atof arclength) (atof radius)));arc internal angle based on string values
	    (setq dist (rtos (* 2 (atof radius) (sin (/ stringO 2))) 2 3))

	       ;APPLY ALL CORRECTIONS AND EXTRACT INFORMATION FROM USER INPUT
(if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
   (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins   (substr bearing (+ dotpt1 2) 2) )
  (if (= (strlen mins) 1)(setq mins (strcat  mins "0")));fix problem with truncating zeros on minutes and seconds
  (setq mins (strcat mins "'"))
  (setq sec  (substr bearing (+ dotpt1 4) 10))
  (if (= (strlen sec) 1) (setq sec (strcat sec "0")))

 

  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))

  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

      
(setq bearing (strcat  deg "d" mins sec))
    ;(setq lbearing bearing)'
	     (setq dist (rtos (atof dist)2 3));remove trailing zeros
  (setq ldist dist )
	    (setq radius (rtos (atof radius)2 3));remove trailing zeros
    (setq lradius radius)




	        ;if nothing at either end of line
	   ;(if (= (or (member setupid pmlist)(member targetid pmlist)(member targetid rmlist)(member setupid rmlist)) nil)(progn

      (SETVAR "CLAYER" rolayer )
      (if (member  (strcat lp1c "," curvecenc "," lp2c "," rolayer) arclist)()
  (progn
       (if (= curverot "ccw") (command "arc" "c" curvecenc lp1c lp2c)(command "arc" "c" curvecenc lp2c lp1c))
          (if (/= sdb "")(setq sdb (strcat " angleAccClass=\"" sdb "\"")))
    (if (/= sdd "")(setq sdd (strcat " distanceAccClass=\"" sdd "\"")))
  (if (/= comment "")(setq ocomment (strcat "><FieldNote>\"" comment "\"</FieldNote></ReducedArcObservation>"))(setq ocomment "/>"))
(SETQ BDINFO (STRCAT "chordAzimuth=\"" xbearing "\" length=\"" arclength "\" radius=\"" radius  "\" rot=\"" curverot "\""  arcType azimuthtype distancetype sdb sdd ocomment))
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

     
      
(if (member (strcat lp1c "," curvecenc "," lp2c "," rolayer) arclist)()(lbarc));label line if not already labelled;label arc using function

))


      
      ;);p

   ; );if nothing at either end of line
     (setq arclist (append arclist (list (strcat lp1c"," curvecenc "," lp2c "," rolayer))))

	    	    ));pif arc


;------------------------------------------------------------------------------POINT--------------------
	(if (/= (vl-string-search "<RedHorizontalPosition" linetext ) nil)
	    (progn
	      
	      

	    (setq stringpos (vl-string-search "setupID" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
            (setq setupids (strcat "IS-" (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8)))))
	    (setq remlist (member setupids islist))
	    (setq setupid (cadr remlist))

	    (setq stringpos (vl-string-search "latitude" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 10)))
            (setq latitude (substr linetext (+ stringpos 11) (-(- wwpos 1)(+ stringpos 9))))

	    (setq stringpos (vl-string-search "longitude" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 11)))
            (setq longitude (substr linetext (+ stringpos 12) (-(- wwpos 1)(+ stringpos 10))))

	    ;(setq stringpos (vl-string-search "class" linetext ))
	    ;(setq wwpos (vl-string-position 34 linetext (+ stringpos 7)))
            ;(setq class (substr linetext (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6))))
	    
	    (if (setq stringpos (vl-string-search "order" linetext )) (progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 7)))
            (setq order (substr linetext (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq order ""))

	    (if (/= (setq stringpos (vl-string-search "currencyDate" linetext )) nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 14)))
            (setq currencydate (substr linetext (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq currencydate ""))

  	    (if (setq stringpos (vl-string-search "horizontalFix" linetext ))(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
            (setq horizontalFix (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq horizontalfix ""))

   	    (if (setq stringpos (vl-string-search "horizontalDatum" linetext ))(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 17)))
            (setq horizontalDatum (substr linetext (+ stringpos 18) (-(- wwpos 1)(+ stringpos 16)))))(setq horizontalDatum ""))

	    ;VIC
	    (if (setq stringpos (vl-string-search "oID" linetext ))(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 5)))
            (setq oid (substr linetext (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4)))))(setq oid ""))

	    (if (setq stringpos (vl-string-search "desc" linetext ))(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
            (setq desc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq desc ""))
	    (setq pmnum desc)

	    (if (setq stringpos (vl-string-search "date" linetext ))(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
            (setq date (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq date ""))

	    (if (setq stringpos (vl-string-search "horizontalAdjustment" linetext ))(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 22)))
            (setq horizontalAdjustment (substr linetext (+ stringpos 23) (-(- wwpos 1)(+ stringpos 21)))))(setq horizontaladjustment ""))

	    

	    

	    
    
	      (if (/= (setq stringpos (vl-string-search "<FieldNote>" linetext )) nil)(progn
											   
(if (setq wwpos (vl-string-position 34 linetext (+ stringpos 12)))
  (progn;if field not contains ""s
    (setq comment (substr linetext (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11))))
    )
  (progn; else use the < to get other end of field note
    (setq <pos (vl-string-position 60 linetext (+ stringpos 11)))
    (setq comment (substr linetext (+ stringpos 12) (-(- <pos 1)(+ stringpos 10))))
    )
  )
)
		
  (setq comment ""))

	    (setq &pos 0)

	    
(while (/=  (setq &pos (vl-string-search "&amp;" comment &pos )) nil) (setq comment (vl-string-subst "&" "&amp;"  comment &pos)
										      &pos (+ &pos 1)))
	    

	    
	    (setq remainlist (member setupid cgpointlist))
            (setq lp1c (cadr remainlist))
	    (setq pmcolist (append pmcolist (list lp1c)))

	    

	    ;(setq remainpmlist (member setupid drawpmlist))
	    ;(setq pmnum (cadr remainpmlist))
	    ;(setq pmstate (caddr remainpmlist))
		  
	    (SETVAR "CLAYER"  "PM" )
	    
	    (setq ,pos1 (vl-string-position 44 lp1c 0))
                 (setq east (atof (substr lp1c 1 ,pos1)))
      		 (setq north (atof (substr lp1c (+ ,pos1 2) 50)))
					   (setq lp1c (list east north))

	      (COMMAND "POINT" lp1c)
	    

		 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
	    (if (/= comment "")(setq ocomment (strcat "><FieldNote>\"" comment "\"</FieldNote></RedHorizontalPosition>"))(setq ocomment "/>"))
	    (if (/= currencydate "")(setq currencydates (strcat " currencyDate=\"" currencydate "\""))(setq currencydates ""))

;(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 (strcat pmnum ",oID=\"" oid "\" latitude=\"" latitude "\" longitude=\"" longitude  "\"  order=\""  order "\" horizontalFix=\""   horizontalFix "\ horizontalAdjustment=\""  horizontalAdjustment "\" horizontalDatum=\""   horizontaldatum  "\" Date=\""  date currencydates "\"/>!type=\"" pmtype pmconditions "\" state=\"" pmstate "\"/>" ))))));@@@@change to xml format


	    (if (/= desc "")(setq descs (strcat "desc=\"" desc "\""))(setq descs ""))
	    (if (/= oid "")(setq oids (strcat " oID=\"" oid "\""))(setq oids ""))
	    (if (/= order "")(setq orders (strcat " order=\"" order "\""))(setq orders ""))
	    (if (/= horizontalFix "")(setq horizontalfixs (strcat " horizontalFix=\"" horizontalFix "\""))(setq horizontalfixs ""))
	    (if (/=  horizontaldatum "")(setq horizontaldatums (strcat " horizontalDatum=\"" horizontaldatum "\""))(setq horizontaldatums ""))
	    (if (/= date "")(setq dates (strcat " date=\"" date "\""))(setq dates ""))
	    (if (/=  horizontaladjustment "")(setq horizontaladjustments (strcat " horizontalAdjustment=\"" horizontaladjustment "\""))(setq horizontaladjustments ""))
				    


	    
(SETQ XDATAS (strcat
	            descs
	             oids
		     " latitude=\"" latitude
		     "\" longitude=\"" longitude "\""
		     orders
		     horizontalfixs
		     horizontaldatums
		     dates
		     currencydates
		     horizontaladjustments
		     ocomment    ));@@@@change to xml format
	    
	    
	    (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 XDATAS)))));@@@@change to xml format
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

	    ;ADD MONUMENT
	    ;get type from RM list

	    (setq remainlist (member setupid rmlist))
	    (setq monline (cadr remainlist))
		   
(if (/= (setq stringpos (vl-string-search "type" monline )) nil)(progn
(setq stringpos (vl-string-search "type" monline ))(setq wwpos (vl-string-position 34 monline (+ stringpos 6)))(setq rmtype (substr monline (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
(if (/= (setq stringpos (vl-string-search "&amp;" rmtype )) nil) (setq rmtype (vl-string-subst "&" "&amp;" rmtype))))(setq rmtype ""))
	    
	     (if (/= (setq stringpos (vl-string-search "state" monline )) nil)(progn
(setq wwpos (vl-string-position 34 monline (+ stringpos 7)))(setq rmstate (substr monline (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq rmstate ""))
	    
(if (/= (setq stringpos (vl-string-search "condition" monline )) nil)(progn
(setq wwpos (vl-string-position 34 monline (+ stringpos 11)))(setq rmcondition (substr monline (+ stringpos 12) (-(- wwpos 1)(+ stringpos 10)))))(setq rmcondition ""))


	    (if (/= (setq stringpos (vl-string-search "desc" monline )) nil)(progn
(setq wwpos (vl-string-position 34 monline (+ stringpos 6)))(setq rmcomment (substr monline (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq rmcomment ""))
(setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" rmcomment &pos )) nil) (setq rmcomment (vl-string-subst "&" "&amp;"  rmcomment &pos)
										      &pos (+ &pos 1)))

(if (/= (setq stringpos (vl-string-search "originSurvey" monline )) nil)(progn
(setq wwpos (vl-string-position 34 monline (+ stringpos 14)))(setq rmrefdp (substr monline (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq rmrefdp ""))


					  
 
   
    ;draw monument info
    
    
  


  
  (SETVAR "CLAYER" "Monument" )

 
 
  (COMMAND "POINT" Lp1c)

  ;check for no values and replace with "none"
    (if (/= rmrefdp "")(setq ormrefdp (strcat " originSurvey=\"" rmrefdp "\" "))(setq ormrefdp ""))
  (if (/= rmcondition "")(setq ormcondition ( strcat " condition=\"" rmcondition "\" "))(setq ormcondition ""))
  (if (/= rmtype "")(setq ormtype ( strcat "type=\"" rmtype "\" "))(setq ormtype ""))

    (if (/= rmcomment "")(setq rmcomments (strcat " desc=\"" rmcomment "\""))(setq rmcomments ""))
    (SETQ PTINFO (STRCAT ormtype "state=\""  rmstate "\"" ormrefdp  ormcondition rmcomments " />" ));Note comment for desc in xml added to distance entry seperated by a space
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 PTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

	    
(setq p1 lp1c)
 (IF (= (SUBSTR pmnum 1 3) "PCM")(LCM));LABEL TYPE AND ORIGIN OF PCM

	    
	    ;rearrage currency date to dd.mm.yyyy format for text output
	    (if (/= currencydate "")(progn
  (setq minuspos1 (vl-string-position 45 currencydate 0))
  (setq minuspos2 (vl-string-position 45 currencydate (+ minuspos1 1)))
  (if  (= minuspos1 4)(progn;rearrage date to year last
				       (setq year  (substr currencydate 1 minuspos1))
				       (if (= (strlen year) 1) (setq year (strcat "0" year)));single digit days
				       (setq month (substr currencydate (+ minuspos1 2) (- (- minuspos2 minuspos1) 1)))
				       (if (= (strlen month) 1) (setq month (strcat "0" month)));single digit days
				       (setq day  (substr currencydate (+ minuspos2 2) 50))
				       (setq currencydate (strcat day "-" month "-" year))
				       ));p&if
  ))

	    

(SETVAR "CLAYER"  "Drafting AFR" )
	    (if (= daf1 "")(setq daf1 "N/A"))
	    (if (= horizontalDatum "")(setq horizontalDatum "N/A")) 

	     (if (= pmboxmark nil)(progn
			 (setq pmboxmark (list (+ maxeast 10) (- minnorth 2)))
			 (setq p10 (list (+ (car pmboxmark) (* 96 th))(+ (cadr pmboxmark) (* -2.5 th))))
			 (setq p11 (list (+ (car pmboxmark) 0)(+ (cadr pmboxmark) (* -2.5 th))))
			 (setq p12 (list (+ (car pmboxmark) (* th 48))(+ (cadr pmboxmark) (* -1.25 th))))
			 (command "rectangle" pmboxmark p10)
			 (command "text" "j" "mc" p12 th "90" "SCHEDULE OF COORDINATES USED")
			 (setq pmboxmark p11)

			 (setq p10 (list (+ (car pmboxmark) (* 96 th))(+ (cadr pmboxmark) (* -2.5 th))))
			 (setq p11 (list (+ (car pmboxmark) 0)(+ (cadr pmboxmark) (* -2.5 th))))
			 (setq p122 (list (+ (car pmboxmark) (* th 48))(+ (cadr pmboxmark) (* -1.25 th))))
			 (command "rectangle" pmboxmark p10)
			 ;removed coordinate info box and put at end
			 (setq pmboxmark p11)
			 ;box corners
			 (setq p10 (list (+ (car pmboxmark) 0)(+ (cadr pmboxmark) (* -2.5 th))));NUM
			 (setq p11 (list (+ (car pmboxmark) (* 22 th))(+ (cadr pmboxmark)  0 )));EAST
			 (setq p12 (list (+ (car pmboxmark) (* 35 th))(+ (cadr pmboxmark) (* -2.5 th))));NORTH
			 (setq p13 (list (+ (car pmboxmark) (* 49 th))(+ (cadr pmboxmark)  0 )));HEIGHT
			 (setq p14 (list (+ (car pmboxmark) (* 55 th))(+ (cadr pmboxmark) (* -2.5 th))));ORDER
			 (setq p15 (list (+ (car pmboxmark) (* 61 th))(+ (cadr pmboxmark)  0 )));SOURCE
			 (setq p16 (list (+ (car pmboxmark) (* 74 th))(+ (cadr pmboxmark) (* -2.5 th))));DATUM
			 (setq p17 (list (+ (car pmboxmark) (* 86 th))(+ (cadr pmboxmark)  0 )));DATE
			 (setq p18 (list (+ (car pmboxmark) (* 96 th))(+ (cadr pmboxmark) (* -2.5 th))))
			 
			 ;draw boxes
			 (command "rectangle" p10 p11)
			 (command "rectangle" p11 p12)
			 (command "rectangle" p12 p13)
			 (command "rectangle" p13 p14)
			 (command "rectangle" p14 p15)
			 (command "rectangle" p15 p16)
			 (command "rectangle" p16 p17)
			 (command "rectangle" p17 p18)
			 
			 ;text insertion points
			 (setq p20 (list (+ (car pmboxmark) (* 11 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p21 (list (+ (car pmboxmark) (* 28.5 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p22 (list (+ (car pmboxmark) (* 42 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p23 (list (+ (car pmboxmark) (* 52 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p24 (list (+ (car pmboxmark) (* 58 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p25 (list (+ (car pmboxmark) (* 67.5 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p26 (list (+ (car pmboxmark) (* 80 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p27 (list (+ (car pmboxmark) (* 91 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 
			 ;create text
			 (command "text" "j" "mc" p20 th "90" "MARK")
			 (command "text" "j" "mc" p21 th "90" "EAST")
			 (command "text" "j" "mc" p22 th "90" "NORTH")
			 (command "text" "j" "mc" p23 th "90" "HEIGHT")
			 (command "text" "j" "mc" p24 th "90" "ORDER")
			 (command "text" "j" "mc" p25 th "90" "SOURCE")
			 (command "text" "j" "mc" p26 th "90" "DATUM")
			 (command "text" "j" "mc" p27 th "90" "DATE")
			 ;reset pm box mark point
			 (setq pmboxmark p10)
			 ));p&if no boxmark


  
  			;box corners
			(setq p10 (list (+ (car pmboxmark) 0)(+ (cadr pmboxmark) (* -2.5 th))));NUM
			 (setq p11 (list (+ (car pmboxmark) (* 22 th))(+ (cadr pmboxmark)  0 )));EAST
			 (setq p12 (list (+ (car pmboxmark) (* 35 th))(+ (cadr pmboxmark) (* -2.5 th))));NORTH
			 (setq p13 (list (+ (car pmboxmark) (* 49 th))(+ (cadr pmboxmark)  0 )));HEIGHT
			 (setq p14 (list (+ (car pmboxmark) (* 55 th))(+ (cadr pmboxmark) (* -2.5 th))));ORDER
			 (setq p15 (list (+ (car pmboxmark) (* 61 th))(+ (cadr pmboxmark)  0 )));SOURCE
			 (setq p16 (list (+ (car pmboxmark) (* 74 th))(+ (cadr pmboxmark) (* -2.5 th))));DATUM
			 (setq p17 (list (+ (car pmboxmark) (* 86 th))(+ (cadr pmboxmark)  0 )));DATE
	                 (setq p18 (list (+ (car pmboxmark) (* 96 th))(+ (cadr pmboxmark) (* -2.5 th))))
			 
			 ;draw boxes
			 (command "rectangle" p10 p11)
			 (command "rectangle" p11 p12)
			 (command "rectangle" p12 p13)
			 (command "rectangle" p13 p14)
			 (command "rectangle" p14 p15)
			 (command "rectangle" p15 p16)
			 (command "rectangle" p16 p17)
			 (command "rectangle" p17 p18)
			 
			 ;text insertion points
			 (setq p20 (list (+ (car pmboxmark) (* 11 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p21 (list (+ (car pmboxmark) (* 28.5 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p22 (list (+ (car pmboxmark) (* 42 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p23 (list (+ (car pmboxmark) (* 52 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p24 (list (+ (car pmboxmark) (* 58 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p25 (list (+ (car pmboxmark) (* 67.5 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p26 (list (+ (car pmboxmark) (* 80 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 (setq p27 (list (+ (car pmboxmark) (* 91 th))(+ (cadr pmboxmark)  (* -1.25 th ))))
			 
			 ;create text
			 (command "text" "j" "mc" p20 th "90" pmnum)
			 (command "text" "j" "mc" p21 th "90" (rtos (atof longitude) 2 3))
			 (command "text" "j" "mc" p22 th "90" (rtos (atof latitude) 2 3))
			 ;(command "text" "j" "mc" p23 th "90" class)
			 (command "text" "j" "mc" p24 th "90" order)
			 (command "text" "j" "mc" p25 th "90" horizontalfix)
			 (command "text" "j" "mc" p26 th "90" horizontaldatum)
			 (command "text" "j" "mc" p27 th "90" currencydate)
			 ;reset pm box mark point
			 (setq pmboxmark p10)

	    (SETQ PMPOS LP1C)
	  (SETQ TEXTPOS (LIST (+ (CAR PMPOS) (* TH 1.5)) (- (CADR PMPOS) (* 1 TH))))

 ; (IF (= PMSTATE "Found")(SETQ PMNUMS (STRCAT PMNUM " FD")))
;	    (IF (= PMSTATE "Placed")(SETQ PMNUMS (STRCAT PMNUM " PL")))
	    (setq pmnums pmnum)

	     ;NSW(IF (= pmstate "Found")  (SETQ PMNUMS (STRCAT PMNUM " FD")))
  (IF (= rmstate "Placed")  (SETQ PMNUMS (STRCAT PMNUM " PL"))(SETQ PMNUMS PMNUM))
		 (COMMAND "TEXT" "J" "BL"  TEXTPOS (* TH 1.4) "90" PMNUMS)
 
  ;NSW(IF (and (/= pmclass "U") (= pmsource "SCIMS" ))(COMMAND "TEXT" "J" "BL"  TEXTPOS (* TH 1.4) "90" "(EST)"))
 

	   (SETVAR "CLAYER"  "Drafting AFR" )
		 (COMMAND "TEXT" "J" "BL"  TEXTPOS (* TH 1.4) "90" PMNUMS)
  (SETQ TEXTPOS (LIST (+ (CAR PMPOS) TH) (+ (CADR PMPOS) (* -1.25 TH))))
  (IF (and (/= class "U") (= horizontalfix "SCIMS" ))(COMMAND "TEXT" "J" "BL"  TEXTPOS (* TH 1.4) "90" "(EST)"))

   (IF (= (SUBSTR PMNUMS 1 3) "PCM")
     (COMMAND "._INSERT" "VPCM" "_S" TH pmpos "0");DRAW PCM BLOCK
     (COMMAND "._INSERT" "VPM" "_S" TH pmpos "0");ELSE PM BLOCK
     )


	    ));p&if reduced point observation


		

	    

		(linereader)
				));p and while not end of observation				 
;);p and if else i.e. <Survey>

  
;Add all monument point offsets
					    
(setq count 1)
  (if (> (length occlist) 0)(progn
(repeat (/ (length occlist)2)
  (setq mon (nth count occlist))
  (setq pnum (nth (- count 1) occlist))
  ;get type
  (setq stringpos (vl-string-search "type" mon ))
	    (setq wwpos (vl-string-position 34 mon (+ stringpos 6)))
            (setq rmtype (substr mon (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
;get state if it exists
  (if (/= (setq stringpos (vl-string-search "state" mon )) nil)(progn
(setq wwpos (vl-string-position 34 mon (+ stringpos 7)))(setq rmstate (substr mon (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq rmstate ""))
  ;get condition
  (if (/= (setq stringpos (vl-string-search "condition" monline )) nil)(progn
(setq wwpos (vl-string-position 34 monline (+ stringpos 11)))(setq rmcondition (substr monline (+ stringpos 12) (-(- wwpos 1)(+ stringpos 10)))))(setq rmcondition ""))
  
;get redef
 (if (/= (setq stringpos (vl-string-search "originSurvey" mon )) nil)(progn
(setq wwpos (vl-string-position 34 mon (+ stringpos 14)))(setq rmrefdp (substr mon (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13))))
   (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" rmrefdp &pos )) nil) (setq rmrefdp (vl-string-subst "&" "&amp;"  rmrefdp &pos)
										      &pos (+ &pos 1)))
)

   (setq rmrefdp ""))

			       ;get description
  (if (/= (setq stringpos (vl-string-search "desc" mon )) nil);if desc exists
    (progn
			        (setq wwpos (vl-string-position 34 mon (+ stringpos 6)))
            (setq rmdesc (substr mon (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
(setq remainlist (member pnum cgpointlist))
            (setq lp1c (cadr remainlist))
	    (setq ,pos1 (vl-string-position 44 lp1c 0))
                 (setq east (atof (substr lp1c 1 ,pos1)))
      		 (setq north (atof (substr lp1c (+ ,pos1 2) 50)))
					   (setq occpnt (list east north))
			       
			       (if (or (and (>= (ascii rmdesc) 48)(<= (ascii rmdesc) 57))(and (= (substr rmdesc 1 1) "(")(>= (ascii (substr rmdesc 2 1)) 48)(<= (ascii (substr rmdesc 2 1)) 57)))
				 (progn;is normal offset either number of bracketted number

				   (if (setq ,pos1 (vl-string-position 44 rmdesc 0))(progn;look for road offset type

						(setq  offset1   (substr rmdesc 1 ,pos1 ))
						(setq  offset2   (substr rmdesc (+ ,pos1 2) 200 ))
				   (setq 2textpos (polar occpnt (* 1.5 pi)  (* th 1.2) ))
						(setvar "clayer" "Occupations")
	(command "point" lp1c)
	(if (/= rmrefdp "")(setq ormrefdp (strcat "\" originSurvey=\"" rmrefdp "\" "))(setq ormrefdp ""))
						(if (/= rmcondition "")(setq ormcondition ( strcat " condition=\"" rmcondition "\" "))(setq ormcondition ""))
  	(SETQ BDINFO (STRCAT "type=\"Occupation\" state=\"" rmstate "\" desc=\"" rmdesc "\"" ormcondition ormrefdp "/>"))
 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

				  

					  (setvar "clayer" "Drafting")
	(COMMAND "TEXT" "J" "BL" lp1c TH "90" offset1)
				   (COMMAND "TEXT" "J" "BL" 2textpos TH "90" offset2)

										       
										       );p
				     (progn;else - i.e. standard offset
	   
				   (setvar "clayer" "Occupations")
				   	(command "point" occpnt)
				   (if (/= rmrefdp "")(setq ormrefdp (strcat "\" originSurvey=\"" rmrefdp "\" "))(setq ormrefdp ""))
  	(SETQ BDINFO (STRCAT "type=\"Occupation\" state=\"" rmstate "\" desc=\"" rmdesc "\""ormrefdp "/>"))
 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
				   ;search line list to find the line or arc which is the offset is a closest match
				   (setq offset (atof rmdesc))
				   (setq offsetdiff 100000000000000000000000000000000000000000000.0)
				   (setq lcount 0)
				   
				   (repeat (length linelist)
				     (setq line (nth lcount linelist))
				     (setq ,pos1 (vl-string-position 44 line 0))
                                     (setq ,pos2 (vl-string-position 44 line (+ ,pos1 1)))
                                     (setq ,pos3 (vl-string-position 44 line (+ ,pos2 1)))
				     (setq ,pos4 (vl-string-position 44 line (+ ,pos3 1)))

				     (setq  x1   (atof(substr line 1 ,pos1 )))
      (setq  y1   (atof(substr line (+ ,pos1 2) (- (- ,pos2 ,pos1) 1)) ))
      (setq  x2  (atof(substr line (+ ,pos2 2) (- (- ,pos3 ,pos2) 1)) ))
      (setq  y2  (atof(substr line (+ ,pos3 2) (- (- ,pos4 ,pos3) 1)) ))
				     (setq p1 (list x1 y1))
				     (setq p2 (list x2 y2))

				     (SETQ LANG (ANGLE P1 P2))
  (SETQ CANG (+ LANG (/ PI 2)))
  (SETQ P4 (POLAR occpnt CANG 50))
  (SETQ P6 (POLAR occpnt (+ CANG PI) 50))
   
   (SETQ interpnt (INTERS P1 P2 P6 P4 nil))					
 
      (SETQ offsetdiffcheck (abs (- (DISTANCE occpnt interpnt) offset)))
				     (if (< offsetdiffcheck offsetdiff)(progn
      (setq p5 interpnt)
  (setq mp (list (/ (+ (car occpnt)(car p5)) 2)(/ (+ (cadr occpnt)(cadr p5)) 2)))
  (setq ang (angle occpnt p5))
  (if (and (> ang  (* 0.5 pi))(< ang (* 1.5 pi)))(setq ang (- ang pi)))
  (if (< ang 0)(setq ang (+ ang (* 2 pi))))
      

  (setq offsetdiff offsetdiffcheck)
  
  ));p&if offsetdiffcheck
				     (setq lcount (+ lcount 1))
  );repeat linelist length

		(setq lcount 0)		   
				    (repeat (length arclist)
				     (setq line (nth lcount arclist))
				     (setq ,pos1 (vl-string-position 44 line 0))
                                     (setq ,pos2 (vl-string-position 44 line (+ ,pos1 1)))
                                     (setq ,pos3 (vl-string-position 44 line (+ ,pos2 1)))
				     (setq ,pos4 (vl-string-position 44 line (+ ,pos3 1)))
				     (setq ,pos5 (vl-string-position 44 line (+ ,pos4 1)))

				     (setq  x1   (atof(substr line 1 ,pos1 )))
      (setq  y1   (atof(substr line (+ ,pos1 2) (- (- ,pos2 ,pos1) 1)) ))
      (setq  cx  (atof(substr line (+ ,pos2 2) (- (- ,pos3 ,pos2) 1)) ))
      (setq  cy  (atof(substr line (+ ,pos3 2) (- (- ,pos4 ,pos3) 1)) ))
      
				     (setq p1 (list x1 y1))
				     (setq cp (list cx cy))
				      (setq offsetdiffcheck (abs(- (abs (- (distance cp occpnt)(distance cp p1)))offset)));check distance from point to cp and p1

				     					
 
      
				     (if (< offsetdiffcheck offsetdiff)(progn
  (setq p5 (polar cp (angle cp occpnt) (distance cp p1)))
  (setq mp (list (/ (+ (car occpnt)(car p5)) 2)(/ (+ (cadr occpnt)(cadr p5)) 2)))
  (setq ang (angle p5 occpnt))
 
  (if (and (> ang  (* 0.5 pi))(< ang (* 1.5 pi)))(setq ang (- ang pi)))
  (if (< ang 0)(setq ang (+ ang (* 2 pi))))
    

  (setq offsetdiff offsetdiffcheck)
  
  

));p&if offsetdiffcheck
				      (setq lcount (+ lcount 1))
  );repeat arclist length


				   
				   				   		(if (> offset (* th 7))(setq tpos mp
													  just "MC"))

					  (if (and (< offset (* th 7))(>= (angle occpnt p5) (* 0.5 pi))(<= (angle occpnt p5)(* 1.5 pi)))(setq tpos p5
																	 just "MR"))
					  (if (and (< offset (* th 7))(or(<= (angle occpnt p5) (* 0.5 pi))(>= (angle occpnt p5)(* 1.5 pi))))(setq tpos p5
																	 just "ML"))
					  (setvar "clayer" "Drafting")

				   (if (/= (setq stringpos (vl-string-search "Clear" rmdesc )) nil) (setq rmdesc (vl-string-subst "Cl" "Clear" rmdesc)))
				   (if (/= (setq stringpos (vl-string-search "Over" rmdesc )) nil) (setq rmdesc (vl-string-subst "Ov" "Over" rmdesc)))
				   
	(COMMAND "TEXT" "J" just tpos TH (ANGTOS ANG 1 4) (strcat "(" (strcase rmdesc) ")"))
				   ));p else &if comma found
					  );p



				 
				 (progn;else queensland style or on point
				     (setvar "clayer" "Occupations")
				   	(command "point" lp1c)
				   (if (/= rmrefdp "")(setq ormrefdp (strcat "\" originSurvey=\"" rmrefdp "\" "))(setq ormrefdp ""))
				(if (/= rmcondition "")(setq ormcondition ( strcat " condition=\"" rmcondition "\" "))(setq ormcondition ""))
  	(SETQ BDINFO (STRCAT "type=\"Occupation\" state=\"" rmstate "\" desc=\"" rmdesc "\"" ormcondition ormrefdp "/>"))
 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

	(setq p1 occpnt
	      rmcomment rmdesc)			  
(lcm)
				   
					  ));p&if 48<ascii<57
      );p if description exists
    
    (progn;else - no description - why would you have occupation with no description Geoffrey John Lawford PS702298H?
      (setq rmdesc "")
(setq remainlist (member pnum cgpointlist))
            (setq lp1c (cadr remainlist))
	    (setq ,pos1 (vl-string-position 44 lp1c 0))
                 (setq east (atof (substr lp1c 1 ,pos1)))
      		 (setq north (atof (substr lp1c (+ ,pos1 2) 50)))
					   (setq occpnt (list east north))
       (setvar "clayer" "Occupations")
				   	(command "point" occpnt)
				   (if (/= rmrefdp "")(setq ormrefdp (strcat "\" originSurvey=\"" rmrefdp "\" "))(setq ormrefdp ""))
      (if (/= rmcondition "")(setq ormcondition ( strcat " condition=\"" rmcondition "\" "))(setq ormcondition ""))
				
  	(SETQ BDINFO (STRCAT "type=\"Occupation\" state=\"" rmstate "\"" ormcondition ormrefdp "/>"))
 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 BDINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

	(setq p1 occpnt
	      rmcomment rmdesc)			  
(lcm)
				   
					  ));p&if description exists
      
    
			       

  (setq count (+ count 2)) 
  );repeat
	));if occlist exists

  (IF (> (LENGTH DRAWNPMLIST) 0)(PROGN
;add line of text about coordinate system to coord box
  (SETVAR "CLAYER" "Drafting" )
  (command "text" "j" "mc" p122 th "90" (strcat  horizontalDatum " COORDINATES ZONE " zone " CSF: " daf1))
))
  ;add pops-------------------------
    (SETVAR "CLAYER" "Drafting" )
(setq count 0)
  (repeat (length poplist)
    (setq ppt (nth count poplist))
   (COMMAND "._INSERT" "POP" "_S" TH ppt "0");changed for BricsCAD
  (SETQ RMB (ENTLAST))
  (SETQ ENTSS (SSADD))
  (SSADD RMB ENTSS)
(COMMAND "DRAWORDER" ENTSS "" "FRONT")
  (setq count (+ count 1))
    );repeat

  ;add pms--------------------------
  (setq count 0)
  (repeat (/ (length drawpmlist)3)
    (setq PPT (nth count drawpmlist))
      (setq remainlist (member PPT cgpointlist))
            (setq PMPOS (cadr remainlist))
 (setq ,pos1 (vl-string-position 44 PMPOS 0))
                 (setq east (atof (substr PMPOS 1 ,pos1)))
      		 (setq north (atof (substr PMPOS (+ ,pos1 2) 50)))
					   (setq PMPOS (list east north))
    
     (setq PMNUM (nth (+ count 1) drawpmlist))

       
  ;DRAW PM BLOCK AND ADD PM TEXT (NOTE POINT IS INSERTED WITH REDUCED POSITION OBSERVATION)
    
   (COMMAND "._INSERT" "PM" "_S" TH pmpos "0");edited for Bricscad courtesy of CAD concepts 
  (SETQ RMB (ENTLAST))
  (SETQ ENTSS (SSADD))
  (SSADD RMB ENTSS)
(COMMAND "DRAWORDER" ENTSS "" "FRONT")
 
    
  (setq count (+ count 3))
    );repeat


  ;check for any remaining RM's that are not connected to the survey, for example RM gone on adjoining boundary.
(setq count 0)
  (repeat (/(length rmlist) 2)
    
    (setq setupid (nth count rmlist))
    (setq remmonlist (member setupid rmlist))

  	    (setq remainlist (member setupid cgpointlist))
            (setq lp1c (cadr remainlist))
	    (setq p1type (caddr remainlist))


												    

												  
												  
(setq monline (cadr remmonlist))

 (if (/= (setq stringpos (vl-string-search "type" monline )) nil)(progn
(setq stringpos (vl-string-search "type" monline ))(setq wwpos (vl-string-position 34 monline (+ stringpos 6)))(setq rmtype (substr monline (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
(if (/= (setq stringpos (vl-string-search "&amp;" rmtype )) nil) (setq rmtype (vl-string-subst "&" "&amp;" rmtype)))
)(setq rmtype ""))

    (if (= (member lp1c pmcolist) nil) (progn
    
 (if (/= (setq stringpos (vl-string-search "state" monline )) nil)(progn
(setq wwpos (vl-string-position 34 monline (+ stringpos 7)))(setq rmstate (substr monline (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq rmstate ""))
(if (/= (setq stringpos (vl-string-search "condition" monline )) nil)(progn
(setq wwpos (vl-string-position 34 monline (+ stringpos 11)))(setq rmcondition (substr monline (+ stringpos 12) (-(- wwpos 1)(+ stringpos 10)))))(setq rmcondition ""))
(if (/= (setq stringpos (vl-string-search "desc" monline )) nil)(progn
(setq wwpos (vl-string-position 34 monline (+ stringpos 6)))(setq rmcomment (substr monline (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq rmcomment ""))
(setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" rmcomment &pos )) nil) (setq rmcomment (vl-string-subst "&" "&amp;"  rmcomment &pos)
										      &pos (+ &pos 1)))

(if (/= (setq stringpos (vl-string-search "originSurvey" monline )) nil)(progn
(setq wwpos (vl-string-position 34 monline (+ stringpos 14)))(setq rmrefdp (substr monline (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq rmrefdp ""))


					  
 
   
    ;draw monument info
    
    
  

  
  (SETVAR "CLAYER" "Monument" )

 (setq ,pos1 (vl-string-position 44 lp1c 0))
                 (setq lp1ce (atof (substr lp1c 1 ,pos1)))
      		 (setq lp1cn (atof (substr lp1c (+ ,pos1 2) 50)))
	    (setq p1 (list lp1ce lp1cn))
 
  (COMMAND "POINT" p1)

  ;check for no values and replace with "none"
    (if (/= rmrefdp "")(setq ormrefdp (strcat " originSurvey=\"" rmrefdp "\" "))(setq ormrefdp ""))
  (if (/= rmcondition "")(setq ormcondition ( strcat " condition=\"" rmcondition "\" "))(setq ormcondition ""))
  (if (/= rmtype "")(setq ormtype ( strcat "type=\"" rmtype "\" "))(setq ormtype ""))

    (if (/= rmcomment "")(setq rmcomments (strcat " desc=\"" rmcomment "\""))(setq rmcomments ""))
    (SETQ PTINFO (STRCAT ormtype "state=\""  rmstate "\"" ormrefdp  ormcondition rmcomments " />" ));Note comment for desc in xml added to distance entry seperated by a space
(SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 PTINFO)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

 (if (vl-string-search " RM" rmcomment )(progn
					   (setq rmornot " RM")
					   (setq rmcomment (vl-string-subst "" "RM" rmcomment))

					   )
    (setq rmornot ""); if not rm corner mark make nil
    )
        
 

  
 (lcm)  




));p&if not a PM or SSM


      
(setq count (+ count 2))
      
     );repeat



  ;;CREATE EASEMENT LEGEND
 ; 
 ; (createeaselegend)
  

  (setvar "clayer" prevlayer )
  (command "zoom" "extents")
(close xmlfile)


  
  )


;-------------------------------------------------------------------------------EXPORT XML---------------------------------------------------------------

(vlax-remove-cmd "XOUT")
(VLAX-ADD-CMD "XOUT" "XOUT" "XOUT")
(DEFUN C:XOUT (/)
       (XOUT)
  )

  (defun XOUT (/)

    (setq prevlayer (getvar "clayer" ))

    (setq cgpl (list));EMPTY CGPOINTLIST
  (setq rolist (list));EMPTY Reference list
  (setq obslist (list));empty observation list
  (setq monlist (list));Empty Monument list
  (setq lotlist (list));Empty lot definition list
  (setq arclist (list));empty observed arc list
  (setq dpl (List));empty datum point list
  (setq pmlist (list));empty pm list
  (setq mplist (list));empty multipartlist
  (setq mpalist (list));empty mulitplart assignment list
  (setq mpolist (list));empty multipart output list
  (setq pflist (list));empty plan features list
  (setq pcount 0);set point count number to 0
  (setq rocount 0);set ref obs count to 0
  (setq moncount 0);set monument count to 0
  (setq cogeocount 1);set coordinate geometry count to 1
  (setq pfwcount 1);set plan features wall count to 1
  (setq pfkcount 1);set plan features kerb count to 1
  (setq pffcount 1);set plan features fence count to 1
  (setq pfbcount 1);set plan features building count to 1
  (setq iblselist (list));irregular boundary start and end points
  (setq ibllist (list));irregular boundary line list for polyline shuffler
  (setq islist (list));instrument station list
  (setq flowarrows "")
  (setq annolist "")
  (setq Roadcount 1)
  (setq Easecounta 1)
    (setq annocount 1)
    (setq easelinklist (list))
    (setq linkno 1)
    (setq resdesclist (list));restriction descripton list
    (setq reslinklist (list));restriction link list
    (setq oclist (list));list of owners corp entitlements
    (setq buildbdys (list));list of strcutural bdy lines
    (setq buildpnts (list));list of structural bdy points

  ;possibly add cgpoints before boundary points

  ;Heirachy
  ;0. Datum Pointsf
  ;1. Boundary lines
  ;2. Boundary Arcs
  ;3. Irregular Boundaries
  ;4. Irregular Right Lines
  ;5. Adjoining lots 
  ;6. Easement lines
  ;7. Easement Arcs
  ;8. PM's
  ;9. Monument points
  ;10. Connection lines (Traverse, Sideshot, Topo)
  ;11. Connection arcs (Traverse, Sideshot, Topo)
  ;12. Plan features - line
  ;13. Plan features - arcs
  ;14. Plan features - Polylines
  ;15. Occupation lines
  ;16. Chainages
  ;17. Admin Sheet  
  ;18. Easement and Restriction link parcel text
  ;19. Easement and Restriction link points
  ;20. Created lots
  

    
     

  
  ;0. Datum Points
    (princ "\nProcessing Datum Points")
(IF (/= (setq bdyline (ssget "_X" '((0 . "POINT") (8 . "Datum Points")))) nil)(progn 
  (setq count 0)
  (repeat (sslength bdyline)


(SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ZA (CDR (ASSOC 210 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ P1 (TRANS P1 ZA 0))
 
    
    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Datum Point with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
 
    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    

    (if (= (member p1s dpl) nil)
      (progn

	(if (/= (caddr p1) 0)
	(setq dpl (append dpl (list p1s)(list xdatai)(list (rtos (caddr p1) 2 6))));if Stratum and rl exists
	(setq dpl (append dpl (list p1s)(list xdatai)(list "NORL")));normal datum point
	  );if stratumswitch
	
	
       	));p&if member

    



    
(SETQ COUNT (+ COUNT 1))
    );r
  )
  (PRINC "\nWARNING No Datum Points found in Project")

  );if datum points found

    ;1.get extinguished boundary lines
    (princ "\nProcessing Extinguished Boundary Lines ")
 
(IF (/= (setq bdyline (ssget "_X" '((0 . "LINE") (8 . "Boundary Extinguished")))) nil)(progn 
  (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	   (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nBoundary Line with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
       (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))

    (if (= (setq remlist(member p1s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is1 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p1s)(list "boundary")(list "existing")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is1 (nth 3 remlist))
       );&if
				   
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is2 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p2s)(list "boundary")(list "existing")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is2 (nth 3 remlist))
       );&if

    (setq rocount (+ rocount 1))
    (setq roline (strcat "<ReducedObservation name=\"OBS-" (rtos rocount 2 0) "\" purpose=\"normal\" setupID=\"IS-" is1 "\" targetSetupID=\"IS-" is2 "\" " xdatai))
    (setq rolist (append rolist (list roline)))
    (setq obslist (append obslist (list (strcat is1 "-" is2))(list ( strcat is2 "-" is1))))
    (setq islist (append islist (list is1 is2)))
    (setq count (+ count 1))
    );r
  );p
  (PRINC "\nWARNING No Boundary Lines found in Project")
  );if
  

;2.get extinguished boundary arcs
  (princ "\nProcessing Extinguished Boundary Arcs ")
(IF (/= (setq bdyline (ssget "_X" '((0 . "ARC") (8 . "Boundary Extinguished")))) nil)(progn 
 

    (setq count 0)
  (repeat (sslength bdyline)


(SETQ CP (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ RADIUS (CDR(ASSOC 40 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG1 (CDR(ASSOC 50 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG2 (CDR(ASSOC 51 (ENTGET (SSNAME bdyline COUNT)))))

  (SETQ P1 (POLAR CP ANG1 RADIUS))
  (SETQ P2 (POLAR CP ANG2 RADIUS))
    ;get xdata
    (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Boundary Arc with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
       (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

 
    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))
    (setq cps (strcat (rtos (cadr cp) 2 4) " " (rtos (car cp) 2 4)))

;centre of arc
   (if (= (setq remlist (member cps cgpl)) nil)
      (progn
	(setq pcount (+ pcount 1))
	(setq cpn (rtos pcount 2 0))
	(setq cgpl (append cgpl (list cps)(list "sideshot")(list "existing")(list (rtos pcount 2 0))))

	)
      (setq cpn (nth 3 remlist))
     )
  
    (setq count (+ count 1))

    (if (= (setq remlist(member p1s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is1 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p1s)(list "boundary")(list "existing")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is1 (nth 3 remlist))
       );&if
				   
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is2 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p2s)(list "boundary")(list "existing")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is2 (nth 3 remlist))
       );&if

     (if (/= (setq stringpos (vl-string-search "rot" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 5)))(setq rot (substr xdatai (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4))))))

    (if (= rot "cw")(progn
		      (setq is1r is1
			is1 is2
			is2 is1r)
		      
      ))
      

    (setq rocount (+ rocount 1))
    (setq roline (strcat "<ReducedArcObservation name=\"AOBS-" (rtos rocount 2 0) "\" purpose=\"normal\" setupID=\"IS-" is1 "\" targetSetupID=\"IS-" is2 "\" " xdatai))
    (setq rolist (append rolist (list roline)))
    (setq obslist (append obslist (list (strcat is1 "-" is2))(list ( strcat is2 "-" is1))))
    (setq islist (append islist (list is1 is2)))
    (setq arclist (append arclist (list (strcat is1 "-" is2))(list cpn) (list (strcat is2 "-" is1))(list cpn)))
    
    );r
    );p
  (PRINC "\nNo Boundary Arcs found in Project")
  );if





  

    ;1.get boundary lines
    (princ "\nProcessing Boundary Lines ")
 
(IF (/= (setq bdyline (ssget "_X" '((0 . "LINE") (8 . "Boundary")))) nil)(progn 
  (setq count 0)
  (princ (strcat "\n" (rtos (sslength bdyline) 2 0 ) "bdy lines found in projet"))
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	   (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nBoundary Line with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
       (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))

    (if (= (setq remlist(member p1s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is1 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p1s)(list "boundary")(list "proposed")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is1 (nth 3 remlist))
       );&if
				   
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is2 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p2s)(list "boundary")(list "proposed")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is2 (nth 3 remlist))
       );&if

    (setq rocount (+ rocount 1))
    (setq roline (strcat "<ReducedObservation name=\"OBS-" (rtos rocount 2 0) "\" purpose=\"normal\" setupID=\"IS-" is1 "\" targetSetupID=\"IS-" is2 "\" " xdatai))
    (setq rolist (append rolist (list roline)))
    (setq obslist (append obslist (list (strcat is1 "-" is2))(list ( strcat is2 "-" is1))))
    (setq islist (append islist (list is1 is2)))
    (setq count (+ count 1))
    );r
  );p
  (PRINC "\nWARNING No Boundary Lines found in Project")
  );if

;2.get boundary arcs
  (princ "\nProcessing Boundary Arcs ")
(IF (/= (setq bdyline (ssget "_X" '((0 . "ARC") (8 . "Boundary")))) nil)(progn 
 

    (setq count 0)
  (repeat (sslength bdyline)


(SETQ CP (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ RADIUS (CDR(ASSOC 40 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG1 (CDR(ASSOC 50 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG2 (CDR(ASSOC 51 (ENTGET (SSNAME bdyline COUNT)))))

  (SETQ P1 (POLAR CP ANG1 RADIUS))
  (SETQ P2 (POLAR CP ANG2 RADIUS))
    ;get xdata
    (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Boundary Arc with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
       (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

 
    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))
    (setq cps (strcat (rtos (cadr cp) 2 4) " " (rtos (car cp) 2 4)))

;centre of arc
   (if (= (setq remlist (member cps cgpl)) nil)
      (progn
	(setq pcount (+ pcount 1))
	(setq cpn (rtos pcount 2 0))
	(setq cgpl (append cgpl (list cps)(list "sideshot")(list "proposed")(list (rtos pcount 2 0))))

	)
      (setq cpn (nth 3 remlist))
     )
  
    (setq count (+ count 1))

    (if (= (setq remlist(member p1s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is1 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p1s)(list "boundary")(list "proposed")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is1 (nth 3 remlist))
       );&if
				   
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is2 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p2s)(list "boundary")(list "proposed")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is2 (nth 3 remlist))
       );&if

     (if (/= (setq stringpos (vl-string-search "rot" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 5)))(setq rot (substr xdatai (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4))))))

    (if (= rot "cw")(progn
		      (setq is1r is1
			is1 is2
			is2 is1r)
		      
      ))
      

    (setq rocount (+ rocount 1))
    (setq roline (strcat "<ReducedArcObservation name=\"AOBS-" (rtos rocount 2 0) "\" purpose=\"normal\" setupID=\"IS-" is1 "\" targetSetupID=\"IS-" is2 "\" " xdatai))
    (setq rolist (append rolist (list roline)))
    (setq obslist (append obslist (list (strcat is1 "-" is2))(list ( strcat is2 "-" is1))))
    (setq islist (append islist (list is1 is2)))
    (setq arclist (append arclist (list (strcat is1 "-" is2))(list cpn) (list (strcat is2 "-" is1))(list cpn)))
    
    );r
    );p
  (PRINC "\nNo Boundary Arcs found in Project")
  );if








  ;3 Irregular Boundary

 ;POLYLINES

    (princ "\nProcessing Irregular Boundaries")
     
 (IF (/= (SETQ LOTS (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Irregular Boundary")))) nil)(progn
(setq count 0)
  (repeat (sslength lots)
    (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))

     (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Irregular Boundary Polyline with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
       (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))
    
;write to list
  
  (setq cogeocount (+ cogeocount 1))
					(setq enlist (entget en))
    ;go through polyline to get points 
    (SETQ PTLIST (LIST))
	    (foreach a enlist
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	     
	    )				;FOREACH



     

    ;delete duplicates from list
    (setq dupcount 0)
    (setq pp "0")
    (setq ppw "500000")
    (repeat  (length ptlist) 
      (setq cpp (strcat (rtos (car (nth dupcount ptlist)) 2 9) "," (rtos (cadr (nth dupcount ptlist)) 2 9)))
      (setq cpw (nth (+ dupcount 1) ptlist)) 
      ;(princ (strcat "\n" pp))
      ;(princ (strcat "\n" cpp))
           
      (if  (= pp cpp) (progn
		
	               (setq ptlist (remove_nth ptlist  dupcount ))
		       
		      		    
							      
		       );p if pp = cp
(progn	       		 
	(setq dupcount (+ dupcount 1))
	))
      (setq pp cpp)
	(setq ppw cpw)
		  
      )

;add start and end point to start end ibl start end list list
    (setq psp (nth 0 ptlist))
   (setq psps (strcat (rtos (cadr psp) 2 4) " " (rtos (car psp) 2 4)))
    (setq p2p (nth 1 ptlist))
    (setq p2ps (strcat (rtos (cadr p2p) 2 4) " " (rtos (car p2p) 2 4)))
    (setq pep (nth (- (length ptlist) 1) ptlist))
   (setq peps (strcat (rtos (cadr pep) 2 4) " " (rtos (car pep) 2 4)))
    (setq p2lastp (nth (- (length ptlist) 2) ptlist))
   (setq p2lastps (strcat (rtos (cadr p2lastp) 2 4) " " (rtos (car p2lastp) 2 4)))
    
    (setq iblselist (append iblselist (list (strcat psps "-" p2ps))(list  xdatai)(list (strcat peps " "))))
    (setq iblselist (append iblselist (list (strcat peps "-" p2lastps))(list  xdatai)(list (strcat psps " "))));reverse of irregular line
        
 (setq count1 0)
 (repeat (length ptlist)
   (setq p1 (nth count1 ptlist))
   (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))

    ;(if (= (setq remlist(member p1s cgpl)) nil)(progn
;				   (setq pcount (+ pcount 1))
;				   (setq cgpl (append cgpl (list p1s)(list "sideshot")(list "proposed")(list (rtos pcount 2 0))))
;				   );p
 ;     );&if
   
   (if (or (/= count1 0) (/= count1 (- (length ptlist)1)))(setq ibllist (append ibllist (list p1s))));add points on line to irregular boundary shuffler
  
  (setq count1 (+ count1 1))
   );r length of ptlist
  (setq count (+ count 1))
  (setq pffcount (+ pffcount 1))
);r length of irregular boundaries
));and if irregular boundary



    
 ;2D & 3DPOLYLINES

     
 (IF (/= (SETQ LOTS (ssget "_X" '((0 . "POLYLINE") (8 . "Irregular Boundary")))) nil)(progn
(setq count 0)
  (repeat (sslength lots)
    (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))

     (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Irregular Boundary Polyline with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
       (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))
    
;write to list
  
  (setq cogeocount (+ cogeocount 1))
					;(setq enlist (entget en))
    ;go through polyline to get points 
 ;   (SETQ PTLIST (LIST))
;	    (foreach a enlist
;	      (if (= 10 (car a))
;
;		(setq PTLIST (append PTLIST (list (cdr a))))
;	      )				;IF
	     
;	    )				;FOREACH

   	    
	    (setq enlist (entget en))
	    (setq ptList (list));EMPTY LIST
	    (setq en2 (entnext en))
	    (setq enlist2 (entget en2))
               
	     (while
	      (not
		(equal (cdr (assoc 0 (entget (entnext en2)))) "SEQEND")
	      )
	      	(if (= (cdr(assoc 70 enlist2)) 16)(progn
	       	 (setq ptList (append ptList (list (cdr (assoc 10 enlist2)))))
		))
		 	       
	       (setq en2 (entnext en2))
	       (setq enlist2 (entget en2))
	       );W
   (setq ptList
			(append ptList (list (cdr (assoc 10 enlist2))))
		 
	       )

     

    ;delete duplicates from list
    (setq dupcount 0)
    (setq pp "0")
    (setq ppw "500000")
    (repeat  (length ptlist) 
      (setq cpp (strcat (rtos (car (nth dupcount ptlist)) 2 9) "," (rtos (cadr (nth dupcount ptlist)) 2 9)))
      (setq cpw (nth (+ dupcount 1) ptlist)) 
      ;(princ (strcat "\n" pp))
      ;(princ (strcat "\n" cpp))
           
      (if  (= pp cpp) (progn
		
	               (setq ptlist (remove_nth ptlist  dupcount ))
		       
		      		    
							      
		       );p if pp = cp
(progn	       		 
	(setq dupcount (+ dupcount 1))
	))
      (setq pp cpp)
	(setq ppw cpw)
		  
      )
    



    

;add start and end point to start end ibl list
    (setq psp (nth 0 ptlist))
   (setq psps (strcat (rtos (cadr psp) 2 4) " " (rtos (car psp) 2 4)))
    (setq p2p (nth 1 ptlist))
    (setq p2ps (strcat (rtos (cadr p2p) 2 4) " " (rtos (car p2p) 2 4)))
    (setq pep (nth (- (length ptlist) 1) ptlist))
   (setq peps (strcat (rtos (cadr pep) 2 4) " " (rtos (car pep) 2 4)))
    (setq p2lastp (nth (- (length ptlist) 2) ptlist))
   (setq p2lastps (strcat (rtos (cadr p2lastp) 2 4) " " (rtos (car p2lastp) 2 4)))
    
    (setq iblselist (append iblselist (list (strcat psps "-" p2ps))(list  xdatai)(list (strcat peps " "))))
    (setq iblselist (append iblselist (list (strcat peps "-" p2lastps))(list  xdatai)(list (strcat psps " "))));reverse of irregular line
        
 (setq count1 0)
 (repeat (length ptlist)
   (setq p1 (nth count1 ptlist))
   (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))

    ;(if (= (setq remlist(member p1s cgpl)) nil)(progn
;				   (setq pcount (+ pcount 1))
;				   (setq cgpl (append cgpl (list p1s)(list "sideshot")(list "proposed")(list (rtos pcount 2 0))))
;				   );p
 ;     );&if
   
   (if (or (/= count1 0) (/= count1 (- (length ptlist)1)))(setq ibllist (append ibllist (list p1s))));add points on line to irregular boundary shuffler
  
  (setq count1 (+ count1 1))
   );r length of ptlist
  (setq count (+ count 1))
  (setq pffcount (+ pffcount 1))
);r length of irregular boundaries
));and if irregular boundary




    

 ;4. Irregular Right Lines
 (princ "\nProcessing Irregular Right Lines")
(IF (/= (setq bdyline (ssget "_X" '((0 . "LINE") (8 . "Irregular Right Lines")))) nil)(progn 
   (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Irregular Right line with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
        (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))

    (if (= (setq remlist(member p1s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is1 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p1s)(list "natural boundary")(list "proposed")(list (rtos pcount 2 0))));changed version 8 of schema was shideshot
				 
				   );p
;else
      (setq is1 (nth 3 remlist))
       );&if
				   
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is2 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p2s)(list "natural boundary")(list "proposed")(list (rtos pcount 2 0))));changed version 8 of schema was shideshot
				   );p
;else
      (setq is2 (nth 3 remlist))
       );&if

    (setq rocount (+ rocount 1))
    (setq roline (strcat "<ReducedObservation name=\"OBS-" (rtos rocount 2 0) "\" purpose=\"topo\" setupID=\"IS-" is1 "\" targetSetupID=\"IS-" is2 "\" " xdatai));changed version 8 of schema was boundary
    (setq rolist (append rolist (list roline)))
    (setq obslist (append obslist (list (strcat is1 "-" is2))(list ( strcat is2 "-" is1))))
    (setq islist (append islist (list is1 is2)))
    (setq count (+ count 1))
    );r
   );p
  (PRINC "\nNo Irregular right lines found in Project")
  );if



    
;5.---------------------------Get Ajoining Lots--------------------------------------------------------
(princ "\nProcessing Adjoining Boundaries")
    (IF (/= (setq lots (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Adjoining Boundary")))) nil)(progn



;MULTIPART FOR ADJOINGING LOTS REMOVED - SHOULD BE NOT NEEDED FOR IT TO WORK ON THEM, NAME IS JUST A TEXT BASED ELEMENT REV1.1
											  
 (setq count 0)
  (repeat (sslength lots)
  
    (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))
    (SETQ CLOSED (CDR(ASSOC 70 (ENTGET (SSNAME lots COUNT)))))
    (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME lots COUNT)))))
    

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Adjoining Lot with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
(if (= (substr xdatai 1 4) "desc")(progn

				    ;Check for easement or road lots note an imported road lot will not have "desc" as the first four letters and not need assignment as below
    (if (/= (setq stringpos (vl-string-search "class" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 7)))(setq lotclass (substr xdatai (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq lotclass ""))
    (if (= lotclass "Road")(progn
			     (setq xdatai (strcat "  <Parcel name=\"ROAD-" (rtos roadcount 2 0) "\" " xdatai))
			     (setq roadcount (+ roadcount 1))
			     )
      )
    (if (= lotclass "Easement")(progn
				 (setq xdatai (strcat "  <Parcel name=\"E" (rtos easecounta 2 0) "\" " xdatai))
				 (setq easecounta (+ easecounta 1))
				 )
      )
    ));p& if desc

    ;check for mutliplart lot
     (if (/= (setq stringpos (vl-string-search "name" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq pclname (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))))

   ;CHECK FOR MULTIPART LOT REMOVED - SEE ABOVE REV 1.1
      
      
      

     

;seperate centrepoint coord out.
    
     (setq !pos1 (vl-string-position 33 xdatai 0))
                      (setq lotc (substr xdatai (+ !pos1 2) 200))
                      (setq xdatai  (substr xdatai 1 !pos1))
  

    (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))
    
  
  
(setq pcount (+ pcount 1))
  (setq cgpl (append cgpl (list lotc)(list "sideshot")(list "existing")(list (rtos pcount 2 0))))
  
;write to list

  
  (setq lotlist (append lotlist (list xdatai)))
  (setq lotlist (append lotlist (list (strcat "    <Center pntRef=\"CGPNT-" (rtos pcount 2 0)"\"/>"))))
  (setq lotlist (append lotlist (list (strcat "    <CoordGeom name=\"CG-" (rtos cogeocount 2 0) "\">"))))
  (setq cogeocount (+ cogeocount 1))
  
  
		(setq enlist (entget en))
    ;go through polyline to get points to check for clockwise direction
    (SETQ CWLIST (LIST))
	    (foreach a enlist
	      (if (= 10 (car a))

		(setq CWLIST (append CWLIST (list (cdr a))))
	      )				;IF
	      
	    )				;FOREACH 			

  (IF (> (lcw CWLIST) 0) (command "pedit" en "r" "" ))
  
    
					(setq enlist (entget en))
    ;go through polyline to get points and bugle factors
    (SETQ PTLIST (LIST))
	    (foreach a enlist
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	      (if (= 42 (car a))
		(setq PTLIST (append PTLIST (LIST (cdr a))))
		);if
	    )				;FOREACH
    (IF (= CLOSED 1)(SETQ PTLIST (APPEND PTLIST (LIST(NTH 0 PTLIST))(LIST(NTH 1 PTLIST)))))
 


    ;SHUFFLER REMOVED - Highly unlikley there will be a Irregular Adjoining Boundary, and if there is make it a normal adjoining bounday REV 1.1
   

    ;(setq ptlist (append ptlist (list(nth 0 ptlist))(list (nth 1 ptlist))));add last point to list to close lot
	

 (setq count1 0)
    (setq repeater (/ (length ptlist) 2))
 (while (/= repeater 1)
   (progn

   (setq p1 (nth count1 ptlist))
   (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
   (setq p2 (nth (+ count1 2) ptlist))
   (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))
   (setq bf (nth (+ count1 1) ptlist))

   ;look for irregular line
   (if (/= (setq remiblselist (member (strcat p1s "-" p2s) iblselist)) nil)
     (progn

       (setq xdatai (nth 1 remiblselist))
        (setq lotlist (append lotlist (list (strcat "      <IrregularLine desc=\"" xdatai "\">"))))
       (setq irbl "")
       (setq pps "")
       (setq iblcount 0)
       (while (/= pps (nth 2 remiblselist))
	 (progn
	 (setq p (nth count1 ptlist))
   (setq ps (strcat (rtos (cadr p) 2 4) " " (rtos (car p) 2 4) " "))
	 (if (/= ps pps)(setq irbl (strcat irbl ps)))
	 (setq pps ps)
	 (setq count1 (+ count1 2))
	 (setq iblcount (+ iblcount 1))
	 ));p&w
       
     (setq ep (strcat (rtos (cadr p) 2 4) " " (rtos (car p) 2 4)))
     
      (if (/= (setq remlist (member p1s cgpl)) nil)(progn
						  (setq p1n (nth 3 remlist))
						  )
       (progn
	 ;(princ (strcat"\n Lot corner not defined by geometery at " (rtos (car p1) 2 6) "," (rtos (cadr p1) 2 6) " orange point placed"))
	 (setq pcount (+ pcount 1))
	 ;(command "color" "30" "point" p1 "color" "bylayer")
	 	 (setq cgpl (append cgpl (list p1s)(list "boundary")(list "existing")(list (rtos pcount 2 0))))
         (setq p1n (rtos pcount 2 0))
	 
	 	 ))
      (if (/= (setq remlist (member ep cgpl)) nil)(progn
						  (setq epn (nth 3 remlist))
						  )
       (progn
	 ;(princ (strcat"\n Lot corner not defined by geometery at " (rtos (car p) 2 6) "," (rtos (cadr p) 2 6) " orange point placed"))
	 (setq pcount (+ pcount 1))
	 ;(command "color" "30" "point" p "color" "bylayer")
	 	 (setq cgpl (append cgpl (list ep)(list "boundary")(list "existing")(list (rtos pcount 2 0))))
         (setq epn (rtos pcount 2 0))
	 
	 	 ))

     		   (setq lotlist (append lotlist (list (strcat "        <Start pntRef=\"CGPNT-"-p1n"\"/>"))))
		   (setq lotlist (append lotlist (list (strcat "        <End pntRef=\"CGPNT-"epn"\"/>"))))
		(setq lotlist (append lotlist (list (strcat "        <PntList2D>" irbl "</PntList2D>"))))
             (setq lotlist (append lotlist (list "      </IrregularLine>")))
     (setq repeater (- repeater (- iblcount 1)))
       (setq count1 (- count1 2))
   
       )
     (progn;else

   ;get edge names
     (if (/= (setq remlist (member p1s cgpl)) nil)(progn
						  (setq p1n (nth 3 remlist))
						  )
       (progn
	 ;(princ (strcat"\n Lot corner not defined by geometery at " (rtos (car p1) 2 6) "," (rtos (cadr p1) 2 6) " orange point placed"))
	 (setq pcount (+ pcount 1))
	 ;(command "color" "30" "point" p1 "color" "bylayer")
	 	 (setq cgpl (append cgpl (list p1s)(list "boundary")(list "existing")(list (rtos pcount 2 0))))
         (setq p1n (rtos pcount 2 0))
	 
	 	 ))
     (if (/= (setq remlist (member p2s cgpl)) nil)(progn
						  (setq p2n (nth 3 remlist))
						  )
       (progn;if not found at corner
	 ;(princ (strcat "\n Lot corner not defined by geometery at" (rtos (car p2) 2 6) "," (rtos (cadr p2) 2 6)" orange point placed"))
	 ;(command "color" "30" "point" p2 "color" "bylayer")
	 (setq pcount (+ pcount 1))
	 (setq cgpl (append cgpl (list p2s)(list "boundary")(list "existing")(list (rtos pcount 2 0))))
         (setq p2n (rtos pcount 2 0))
	 
	 	 ))
(if (/= p1n p2n)(progn ;is not the same point

		  (if (= (member (strcat p1n "-" p2n) obslist) nil)
		    (progn ;check if lot line has edge observation
		     ; (princ (strcat "\n Lot edge not defined by observation from " (rtos (car p1) 2 3) "," (rtos (cadr p1) 2 3) " to " (rtos (car p2) 2 3) "," (rtos (cadr p2) 2 3) " orange line placed, suggest checking"))
		;	(command "color" "30" "line" p1 p2"" "color" "bylayer")
		      ));p & if no observation
		  
   (if  (= bf 0.0)(progn ;is a line
		   (setq lotlist (append lotlist (list "      <Line>")))
		   (setq lotlist (append lotlist (list (strcat "        <Start pntRef=\"CGPNT-"p1n"\"/>"))))
		   (setq lotlist (append lotlist (list (strcat "        <End pntRef=\"CGPNT-"p2n"\"/>"))))
		   (setq lotlist (append lotlist (list "      </Line>")))
		   );p
     (progn;is an arc

       
       ;FIGURE OUT centrePOINT
       
               (setq DI (DISTANCE P1 P2))
               (SETQ AZ (ANGLE P1 P2))
                (SETQ MP (POLAR P1 AZ (/ DI 2)))
       		(SETQ H (/ (* BF DI) 2))
		(SETQ AMP (POLAR MP (+ AZ (* 1.5 PI)) H))
		(SETQ X1 (CAR P1))
		(SETQ Y1 (CADR P1))
		(SETQ X2 (CAR AMP))
		(SETQ Y2 (CADR AMP))
		(SETQ X3 (CAR P2))
		(SETQ Y3 (CADR P2))
		(SETQ MA (/ (- Y2 Y1) (- X2 X1)))
		(SETQ MB (/ (- Y3 Y2) (- X3 X2)))
		(SETQ  RPX (/ (- (+ (* MA MB (- Y1 Y3)) (* MB (+ X1 X2)))  (* MA (+ X2 X3)) )(* 2 (- MB MA))))
		(SETQ  RPY (+ (* (* (/ 1 MA) -1) (- RPX (/ (+ X1 X2) 2)))  (/ (+ Y1 Y2) 2)))
       (setq radius (distance amp (list rpx rpy)))
       (SETQ CP (LIST RPX RPY))
		(SETQ CPS (STRCAT (RTOS (CAdR CP) 2 4) " " (RTOS (CAR CP) 2 4)))

    
;note all that work to create the centerpoint should be redundant, as there should be an observation over this arc so....
;       (if (or (setq remlist (member (strcat p1n "-" p2n) arclist))(setq remlist (member (strcat p2n "-" p1n) arclist)))
;  (progn
;    (setq cpn (cadr remlist))
;    )
;  (progn
 ;   (princ (strcat "\n Arc not defined by observation from " (rtos (car p1) 2 3) "," (rtos (cadr p1) 2 3) " to " (rtos (car p2) 2 3) "," (rtos (cadr p2) 2 3) " orange line placed, suggest checking"))
;	(command "color" "30" "line" p1 p2"" "color" "bylayer")

       ;GET centrePPOINT NUMBER
(if (/= (setq remlist (member CPS cgpl)) nil)(progn
						  (setq CPn (nth 3 remlist))
						  )
  
      (progn
	 ;(princ (strcat "\n Arc centre not yet defined by geometery at " CPS))
	 (setq pcount (+ pcount 1))
	 (setq cgpl (append cgpl (list CPS)(list "sideshot")(list "existing")(list (rtos pcount 2 0))))
         (setq CPn (rtos pcount 2 0))
	 
	 	 ))

    
     ;  	)
;	 );if
       
(if (> bf 0)(setq rot "ccw")(setq rot "cw"))
       
       		   (setq lotlist (append lotlist (list (strcat"      <Curve radius=\"" (rtos radius 2 6) "\" rot=\"" rot "\">"))))
		   (setq lotlist (append lotlist (list (strcat "        <Start pntRef=\"CGPNT-"p1n"\"/>"))))
                   (setq lotlist (append lotlist (list (strcat "        <Center pntRef=\"CGPNT-"cpn"\"/>"))))
		   (setq lotlist (append lotlist (list (strcat "        <End pntRef=\"CGPNT-"p2n"\"/>"))))
		   (setq lotlist (append lotlist (list "      </Curve>")))


       );p
     );if LINE OR ARC
		 
		  
		  
   ));p&if p1n is not p2n

       (setq count1 (+ count1 2))
      (setq repeater (- repeater 1))
     
     ));end else and if not irregular
     

    

     
   ));while repeater length of ptlist
  (setq lotlist (append lotlist (list "    </CoordGeom>")))
  (setq lotlist (append lotlist (list "  </Parcel>")))
  (setq count (+ count 1))
);r length of lots
);p
  (PRINC "\nWARNING No Adjoining Lots found in Project")
  );if
 
  ;if lots found





  


     ;6.get easement lines
  (princ "\nProcessing Easement Lines")
(IF (/= (setq bdyline (ssget "_X" '((0 . "LINE") (8 . "Easement")))) nil)(progn 
   (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Easement Line with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
        (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))

    (if (= (setq remlist(member p1s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is1 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p1s)(list "boundary")(list "proposed")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is1 (nth 3 remlist))
       );&if
				   
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is2 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p2s)(list "boundary")(list "proposed")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is2 (nth 3 remlist))
       );&if

    (setq rocount (+ rocount 1))
    (setq roline (strcat "<ReducedObservation name=\"OBS-" (rtos rocount 2 0) "\" purpose=\"normal\" setupID=\"IS-" is1 "\" targetSetupID=\"IS-" is2 "\" " xdatai))
    (setq rolist (append rolist (list roline)))
    (setq obslist (append obslist (list (strcat is1 "-" is2))(list ( strcat is2 "-" is1))))
    (setq islist (append islist (list is1 is2)))
    (setq count (+ count 1))
    );r
   );p
  (PRINC "\nNo Easement Lines found in Project")
  );if
  

;7.get easement arcs
 (princ "\nProcessing Easement Arcs")
(IF (/=  (setq bdyline (ssget "_X" '((0 . "ARC") (8 . "Easement")))) nil)(progn 
 
   (setq count 0)
  (repeat (sslength bdyline)


(SETQ CP (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ RADIUS (CDR(ASSOC 40 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG1 (CDR(ASSOC 50 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG2 (CDR(ASSOC 51 (ENTGET (SSNAME bdyline COUNT)))))

  (SETQ P1 (POLAR CP ANG1 RADIUS))
  (SETQ P2 (POLAR CP ANG2 RADIUS))
    ;get xdata
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	   (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Easement Arc with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
        (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

 
    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))
    (setq cps (strcat (rtos (cadr cp) 2 4) " " (rtos (car cp) 2 4)))

;centre of arc
    (if (= (setq remlist (member cps cgpl)) nil)
      (progn
	(setq pcount (+ pcount 1))
	(setq cpn (rtos pcount 2 0))
	(setq cgpl (append cgpl (list cps)(list "sideshot")(list "proposed")(list (rtos pcount 2 0))))

	)
      (setq cpn (nth 3 remlist))
     )

  
    (setq count (+ count 1))

    (if (= (setq remlist(member p1s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is1 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p1s)(list "boundary")(list "proposed")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is1 (nth 3 remlist))
       );&if
				   
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is2 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p2s)(list "boundary")(list "proposed")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is2 (nth 3 remlist))
       );&if

     (if (/= (setq stringpos (vl-string-search "rot" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 5)))(setq rot (substr xdatai (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4))))))

    (if (= rot "cw")(progn
		      (setq is1r is1
			is1 is2
			is2 is1r)
		      
      ))
      

    (setq rocount (+ rocount 1))
    (setq roline (strcat "<ReducedArcObservation name=\"AOBS-" (rtos rocount 2 0) "\" purpose=\"normal\" setupID=\"IS-" is1 "\" targetSetupID=\"IS-" is2 "\" " xdatai))
    (setq rolist (append rolist (list roline)))
    (setq obslist (append obslist (list (strcat is1 "-" is2))(list ( strcat is2 "-" is1))))
    (setq islist (append islist (list is1 is2)))
    (setq arclist (append arclist (list (strcat is1 "-" is2))(list cpn) (list (strcat is2 "-" is1))(list cpn)))

    );r
   );p
  (PRINC "\nNo Easement Arcs found in Project")
  );if

    
;9.get Monuments points
  (princ "\nProcessing Monuments")
(IF (/= (setq bdyline (ssget "_X" '((0 . "POINT") (8 . "Monument")))) nil)(progn 
  (setq count 0)
  (repeat (sslength bdyline)


(SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
(SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ ZA (CDR (ASSOC 210 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ P1 (TRANS P1 ZA 0))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR RM point with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
       (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

     (if (/= (setq stringpos (vl-string-search "state" xdatai )) nil)(progn
	(setq wwpos (vl-string-position 34 xdatai (+ stringpos 7)))(setq rmstate (substr xdatai (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq rmstate ""))


    (if (/= (setq stringpos (vl-string-search "type" xdatai )) nil)(progn
	(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq rmtype (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq rmtype ""))

    
 
 
    
    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    

    (if (= (member p1s cgpl) nil)(progn
				    (setq pcount (+ pcount 1))
    (setq cgpl (append cgpl (list p1s)(list "reference")))
    (if (or (= rmstate "Placed")(= rmstate "New"))(setq cgpl (append cgpl (list "proposed")(list (rtos pcount 2 0))));if new
    (setq cgpl (append cgpl (list "existing")(list (rtos pcount 2 0)))));else
				   (setq pntref (rtos pcount 2 0))
				   );if not member
      (progn;else i.e. found in list i.e. mark on corner
(setq pntref (nth 3 (member p1s cgpl)))
	))

    (if (and (or (= rmtype "SSM")(= rmtype "PM")(= rmtype "TS")(= rmtype "MM")(= rmtype "GB")(= rmtype "CP")(= rmtype "CR"))
	     (setq pmnth (vl-position pntref monlist)))
      (progn
	(setq monlist (remove_nth  monlist pmnth))
	(setq monlist (remove_nth monlist pmnth ))
	)
      )
	
	   
    (setq moncount (+ moncount 1))
    (setq monline (strcat "<Monument name=\"MON-" (rtos moncount 2 0) "\" pntRef=\"CGPNT-" pntref  "\" " xdatai))
    (setq monlist (append monlist (list pntref)(list monline)))

    
  
    (setq count (+ count 1))

    );r
  );p
  (PRINC "\nWARNING No Reference Marks found in Project")
  );if



    ;8.get control points
  (princ "\nProcessing Control Points")
(IF (/= (setq bdyline (ssget "_X" '((0 . "POINT") (8 . "PM")))) nil)(progn 
  (setq count 0)
  (repeat (sslength bdyline)


(SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ ZA (CDR (ASSOC 210 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ P1 (TRANS P1 ZA 0))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Control point with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
    (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

     

   (if (/= (setq stringpos (vl-string-search "oid" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 5)))(setq pmnum (substr xdatai (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4))))))
 
    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    

    (if (= (member p1s cgpl) nil)
      (progn
	(setq rocount (+ rocount 1))
	(setq pcount (+ pcount 1))

	(setq cgpl (append cgpl (list p1s)(list "control")(list "existing")(list (rtos pcount 2 0))))

        (setq rolist (append rolist ( list (strcat "<RedHorizontalPosition name=\"HP-" (rtos rocount 2 0) "\" setupID=\"IS-" (rtos pcount 2 0) "\" " xdatai))))
  (setq pmlist (append pmlist (list p1s ) (list pmnum)))
    (setq count (+ count 1))
	));p&if not member

    ;substitute referece for control if already in CGPL (all should be)
    (if (member p1s cgpl)
      (progn
	(setq cgpl (replaceitem (+ 2(- (length cgpl)(length (member p1s cgpl)))) "control" cgpl))
      	(setq rocount (+ rocount 1))
	
      (setq rolist (append rolist ( list (strcat "<RedHorizontalPosition name=\"HP-" (rtos rocount 2 0) "\" setupID=\"IS-" (nth 3 (member p1s cgpl)) "\" " xdatai))))
        (setq pmlist (append pmlist (list p1s ) (list pmnum)))
    (setq count (+ count 1))
      ));p&if member
      

      

    (setq pntref (rtos pcount 2 0))

 ;removed adding pm to mon list
    
    );r
  );p
  (PRINC "\nWARNING No Control Points (PM/SSM's) found in Project")
  );if

   ;10.get connection lines
  (princ "\nProcessing Connection Lines")
(IF (/= (setq bdyline (ssget "_X" '((0 . "LINE") (8 . "Topo,Traverse,Sideshot")))) nil)(progn 
   (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ LAYER (strcase (CDR(ASSOC 8 (ENTGET (SSNAME bdyline COUNT)))) t))
    (IF (= LAYER "topo" )(setq pntsurv "sideshot")(setq pntsurv "traverse"))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Connection Line with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
       (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))

    (if (= (setq remlist(member p1s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is1 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p1s)(list pntsurv)(list "proposed")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is1 (nth 3 remlist))
       );&if
				   
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is2 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p2s)(list pntsurv)(list "proposed")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is2 (nth 3 remlist))
       );&if

    (setq rocount (+ rocount 1))
    (setq roline (strcat "<ReducedObservation name=\"OBS-" (rtos rocount 2 0) "\" purpose=\"" LAYER "\" setupID=\"IS-" is1 "\" targetSetupID=\"IS-" is2 "\" " xdatai))
    (setq rolist (append rolist (list roline)))
    (setq islist (append islist (list is1 is2)))
    (if (= layer "topo" )
      (setq obslist (append obslist (list (strcat is1 "-" is2))(list ( strcat is2 "-" is1))))
      )
    (setq count (+ count 1))
    );r
   );p
  (PRINC "\nWARNING No Connection Lines found in Project")
  );if

;11.get Connection arcs
  (princ "\nProcessing Connection Arcs")
(IF (/= (setq bdyline (ssget "_X" '((0 . "ARC") (8 . "Topo,Traverse,Sideshot")))) nil)(progn 
   (setq count 0)
  (repeat (sslength bdyline)


(SETQ CP (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ RADIUS (CDR(ASSOC 40 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG1 (CDR(ASSOC 50 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG2 (CDR(ASSOC 51 (ENTGET (SSNAME bdyline COUNT)))))
(SETQ LAYER  (CDR(ASSOC 8 (ENTGET (SSNAME bdyline COUNT)))) )
    (IF (= LAYER "Topo" )(setq pntsurv "sideshot")(setq pntsurv "traverse"))
    
  (SETQ P1 (POLAR CP ANG1 RADIUS))
  (SETQ P2 (POLAR CP ANG2 RADIUS))
    ;get xdata
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Connection Arc with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
       (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))
 
    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))
    (setq cps (strcat (rtos (cadr cp) 2 4) " " (rtos (car cp) 2 4)))

;centre of arc
  ;  (if (= (member cps cgpl) nil)
  ;    (progn
	;(setq pcount (+ pcount 1))
	;(setq cgpl (append cgpl (list cps)(list "sideshot")(list "proposed")(list (rtos pcount 2 0))))

	;)
      ;)
  
    

    (if (= (setq remlist(member p1s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is1 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p1s)(list pntsurv)(list "proposed")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is1 (nth 3 remlist))
       );&if
				   
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq is2 (rtos pcount 2 0))
				   (setq cgpl (append cgpl (list p2s)(list pntsurv)(list "proposed")(list (rtos pcount 2 0))))
				   );p
;else
      (setq is2 (nth 3 remlist))
       );&if

     (if (/= (setq stringpos (vl-string-search "rot" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 5)))(setq rot (substr xdatai (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4))))))

    (if (= rot "cw")(progn
		      (setq is1r is1
			is1 is2
			is2 is1r)
		      
      ))
      

    (setq rocount (+ rocount 1))
    (setq roline (strcat "<ReducedArcObservation name=\"ABOS-" (rtos rocount 2 0) "\" purpose=\"" LAYER "\" setupID=\"IS-" is1 "\" targetSetupID=\"IS-" is2 "\" " xdatai))
    (setq rolist (append rolist (list roline)))
    (setq islist (append islist (list is1 is2)))
    (if (= layer "Topo" )
      (setq obslist (append obslist (list (strcat is1 "-" is2))(list ( strcat is2 "-" is1))))
      )
  
    (setq count (+ count 1))

    );r
   );p
  (PRINC "\nNo Conection Arcs found in Project")
  );if

   



   ;12.Plan features - Lines
(princ "\nProcessing Plan Features, Lines, ")
  (IF (/= (setq bdyline (ssget "_X" '((0 . "LINE") (8 . "Occupation Fences,Occupation Building Return,Occupation Timber Walls,Occupation Walls,Occupation Centreline,Occupation Railway,Occupation Rockwall,Occupation Hedge,Occupation Other,Occupation Gate,Occupation Not Fenced")))) nil)(progn 
  (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ LAYER (CDR(ASSOC 8 (ENTGET (SSNAME bdyline COUNT)))))

      (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL)(SETQ XDATAI (substr layer 12 200))
    (PROGN
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))))
        (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))
    
    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))

    (if (= (setq remlist(member p1s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq cgpl (append cgpl (list p1s)(list "monument")(list "existing")(list (rtos pcount 2 0))))
				   );p

       );&if
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq cgpl (append cgpl (list p2s)(list "monument")(list "existing")(list (rtos pcount 2 0))))
				   );p
       );&if

         (if (/= (setq remlist (member p1s cgpl)) nil)(progn
						  (setq p1n (nth 3 remlist))
						  ))
	  (if (/= (setq remlist (member p2s cgpl)) nil)(progn
						  (setq p2n (nth 3 remlist))
						  ))

    
    (if (= layer  "Occupation Walls") (setq prepf "WALL-"))
(if (= layer  "Occupation Building Return") (setq prepf "BRT-"))
(if (= layer  "Occupation Timber Walls") (setq prepf "TWALL-"))
(if (= layer  "Occupation Fences") (setq prepf "FEN-"))
(if (= layer  "Occupation Kerbs") (setq prepf "KERB-"))
(if (= layer  "Occupation Centreline") (setq prepf "CNTL-"))
(if (= layer  "Occupation Railway") (setq prepf "RAIL-"))
(if (= layer  "Occupation Rockwall") (setq prepf "RWALL-"))
(if (= layer  "Occupation Hedge") (setq prepf "HDG-"))
(if (= layer  "Occupation Other") (setq prepf "OTH-"))
(if (= layer  "Occupation Gate") (setq prepf "GATE-"))
(if (= layer  "Occupation Not Fenced") (setq prepf "NSMB-"))
    



    
    (setq pflist (append pflist (list (strcat"  <PlanFeature name=\"" prepf (rtos pffcount 2 0) "\" desc=\"" xdatai "\">"))
			 (list (strcat "    <CoordGeom name=\"CG-"(rtos cogeocount 2 0) "\">"))
			 (list "      <Line>")
			 (list (strcat "      <Start pntRef=\"CGPNT-"p1n"\"/>"))
			 (list (strcat "      <End pntRef=\"CGPNT-"p2n"\"/>"))
			 (list "      </Line>")
			 (list "    </CoordGeom>")
			 (list "  </PlanFeature>")
			 ))
	    (setq pffcount (+ pffcount 1))
	    (setq cogeocount (+ cogeocount 1))
	    (setq count (+ count 1))
    );r
  );p
     );if


     ;13.Plan features - Arcs
(princ "Arcs, ")
  (IF (/= (setq bdyline (ssget "_X" '((0 . "ARC") (8 . "Occupation Fences,Occupation Building Return,Occupation Timber Walls,Occupation Walls,Occupation Centreline,Occupation Railway,Occupation Rockwall,Occupation Hedge,Occupation Other,Occupation Gate,Occupation Not Fenced")))) nil)(progn
 (setq count 0)
  (repeat (sslength bdyline)
(SETQ CP (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ RADIUS (CDR(ASSOC 40 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG1 (CDR(ASSOC 50 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG2 (CDR(ASSOC 51 (ENTGET (SSNAME bdyline COUNT)))))
  
  (SETQ P1 (POLAR CP ANG1 RADIUS))
  (SETQ P2 (POLAR CP ANG2 RADIUS))

    (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
(SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL)(SETQ XDATAI (substr layer 12 200))
    (PROGN
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))))
        (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

    
    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))
    (setq cps (strcat (rtos (cadr cp) 2 4) " " (rtos (car cp) 2 4)))
;centre of arc
    (if (= (member cps cgpl) nil)
      (progn
	(setq pcount (+ pcount 1))
	(setq cgpl (append cgpl (list cps)(list "sideshot")(list "existing")(list (rtos pcount 2 0))))
	)
      )
    (if (= (setq remlist(member p1s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq cgpl (append cgpl (list p1s)(list "monument")(list "existing")(list (rtos pcount 2 0))))
				   );p
      );&if
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq cgpl (append cgpl (list p2s)(list "monument")(list "existing")(list (rtos pcount 2 0))))
				   );p
       );&if
          (if (/= (setq remlist (member p1s cgpl)) nil)(progn
						  (setq p1n (nth 3 remlist))
						  ))
         (if (/= (setq remlist (member p2s cgpl)) nil)(progn
						  (setq p2n (nth 3 remlist))
						  ))
         (if (/= (setq remlist (member cps cgpl)) nil)(progn
						  (setq cpn (nth 3 remlist))
						  ))

    
    
    (if (= layer  "Occupation Walls") (setq prepf "WALL-"))
(if (= layer  "Occupation Building Return") (setq prepf "BRT-"))
(if (= layer  "Occupation Timber Walls") (setq prepf "TWALL-"))
(if (= layer  "Occupation Fences") (setq prepf "FEN-"))
(if (= layer  "Occupation Kerbs") (setq prepf "KERB-"))
(if (= layer  "Occupation Centreline") (setq prepf "CNTL-"))
(if (= layer  "Occupation Railway") (setq prepf "RAIL-"))
(if (= layer  "Occupation Rockwall") (setq prepf "RWALL-"))
(if (= layer  "Occupation Hedge") (setq prepf "HDG-"))
(if (= layer  "Occupation Other") (setq prepf "OTH-"))
(if (= layer  "Occupation Gate") (setq prepf "GATE-"))
(if (= layer  "Occupation Not Fenced") (setq prepf "NSMB-"))

    
    (setq pflist (append pflist (list (strcat"  <PlanFeature name=\"" prepf (rtos pffcount 2 0) "\" desc=\"" xdatai "\">"))
			 (list (strcat "    <CoordGeom name=\"CG-"(rtos cogeocount 2 0) "\">"))
			 (list (strcat"      <Curve radius=\"" (rtos radius 2 6) "\" rot=\"ccw\">"))
			 (list (strcat "      <Start pntRef=\"CGPNT-"p1n"\"/>"))
			 (list (strcat "      <Center pntRef=\"CGPNT-"cpn"\"/>"))
			 (list (strcat "      <End pntRef=\"CGPNT-"p2n"\"/>"))
			 (list "      </Curve>")
			 (list "    </CoordGeom>")
			 (list "  </PlanFeature>")
			 ))
	    (setq pffcount (+ pffcount 1))
	    (setq cogeocount (+ cogeocount 1))
	    (setq count (+ count 1))
    )));r

    
  ;14.Plan features - Polyline
    (princ "PolyLines, ")
 (IF (/= (setq lots (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Occupation Fences,Occupation Building Return,Occupation Timber Walls,Occupation Walls,Occupation Centreline,Occupation Railway,Occupation Rockwall,Occupation Hedge,Occupation Other,Occupation Gate,Occupation Not Fenced")))) nil)(progn
(setq count 0)
  (repeat (sslength lots)
    (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))
    (SETQ layer (CDR(ASSOC 8 (ENTGET (SSNAME lots COUNT)))))
    
   (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL)(SETQ XDATAI (substr layer 12 200))
    (PROGN
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))))
        (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))
    (SETQ CLOSED (CDR(ASSOC 70 (ENTGET (SSNAME lots COUNT)))))
;write to list

     
    (if (= layer  "Occupation Walls") (setq prepf "WALL-"))
(if (= layer  "Occupation Building Return") (setq prepf "BRT-"))
(if (= layer  "Occupation Timber Walls") (setq prepf "TWALL-"))
(if (= layer  "Occupation Fences") (setq prepf "FEN-"))
(if (= layer  "Occupation Kerbs") (setq prepf "KERB-"))
(if (= layer  "Occupation Centreline") (setq prepf "CNTL-"))
(if (= layer  "Occupation Railway") (setq prepf "RAIL-"))
(if (= layer  "Occupation Rockwall") (setq prepf "RWALL-"))
(if (= layer  "Occupation Hedge") (setq prepf "HDG-"))
(if (= layer  "Occupation Other") (setq prepf "OTH-"))
(if (= layer  "Occupation Gate") (setq prepf "GATE-"))
(if (= layer  "Occupation Not Fenced") (setq prepf "NSMB-"))
    
  (setq pflist (append pflist (list (strcat"  <PlanFeature name=\"" prepf (rtos pffcount 2 0) "\" desc=\""xdatai"\">"))))
  (setq pflist (append pflist (list (strcat "    <CoordGeom name=\"CG-" (rtos cogeocount 2 0) "\">"))))
  (setq cogeocount (+ cogeocount 1))
					(setq enlist (entget en))
    ;go through polyline to get points and bugle factors
    (SETQ PTLIST (LIST))
	    (foreach a enlist
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	      (if (= 42 (car a))
		(setq PTLIST (append PTLIST (LIST (cdr a))))
		);if
	    )				;FOREACH 			
(IF (= CLOSED 1)(SETQ PTLIST (APPEND PTLIST (LIST(NTH 0 PTLIST))(LIST(NTH 1 PTLIST)))))

    (setq count1 0)
 (repeat (- (/ (length ptlist) 2) 1)
   (setq p1 (nth count1 ptlist))
   (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
   (setq p2 (nth (+ count1 2) ptlist))
   (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))
   (setq bf (nth (+ count1 1) ptlist))
   ;get edge names
     (if (/= (setq remlist (member p1s cgpl)) nil)(progn
						  (setq p1n (nth 3 remlist))
						  )
       (progn
	 ;(princ (strcat"\n Lot corner not defined by geometery at " p1s))
         (setq pcount (+ pcount 1))
	 (setq cgpl (append cgpl (list p1s)(list "monument")(list "existing")(list (rtos pcount 2 0))))
         (setq p1n (rtos pcount 2 0))
	 	 ))
     (if (/= (setq remlist (member p2s cgpl)) nil)(progn
						  (setq p2n (nth 3 remlist))
						  )
       (progn;if not found at corner
	 ;(princ (strcat "\n Lot corner yet not defined by geometery at " p2s))
	 (setq pcount (+ pcount 1))
	 (setq cgpl (append cgpl (list p2s)(list "monument")(list "existing")(list (rtos pcount 2 0))))
         (setq p2n (rtos pcount 2 0))
	 	 ))
   (if (= bf 0.0)(progn ;is a line
		   (setq pflist (append pflist (list "      <Line>")))
		   (setq pflist (append pflist (list (strcat "        <Start pntRef=\"CGPNT-"p1n"\"/>"))))
		   (setq pflist (append pflist (list (strcat "        <End pntRef=\"CGPNT-"p2n"\"/>"))))
		   (setq pflist (append pflist (list "      </Line>")))
		   );p
     (progn;is an arc
       ;FIGURE OUT centrePOINT
               (setq DI (DISTANCE P1 P2))
               (SETQ AZ (ANGLE P1 P2))
                (SETQ MP (POLAR P1 AZ (/ DI 2)))
       		(SETQ H (/ (* BF DI) 2))
		(SETQ AMP (POLAR MP (+ AZ (* 1.5 PI)) H))
		(SETQ X1 (CAR P1))
		(SETQ Y1 (CADR P1))
		(SETQ X2 (CAR AMP))
		(SETQ Y2 (CADR AMP))
		(SETQ X3 (CAR P2))
		(SETQ Y3 (CADR P2))
		(SETQ MA (/ (- Y2 Y1) (- X2 X1)))
		(SETQ MB (/ (- Y3 Y2) (- X3 X2)))
		(SETQ  RPX (/ (- (+ (* MA MB (- Y1 Y3)) (* MB (+ X1 X2)))  (* MA (+ X2 X3)) )(* 2 (- MB MA))))
		(SETQ  RPY (+ (* (* (/ 1 MA) -1) (- RPX (/ (+ X1 X2) 2)))  (/ (+ Y1 Y2) 2)))
       (setq radius (distance amp (list rpx rpy)))
       (SETQ CP (LIST RPX RPY))
		(SETQ CPS (strcat (rtos (cadr cp) 2 4) " " (rtos (car cp) 2 4)))
       ;GET centrePPOINT NUMBER
(if (/= (setq remlist (member CPS cgpl)) nil)(progn
						  (setq CPn (nth 3 remlist))
						  )
       (progn
	 ;(princ (strcat "\n Arc centre not yet defined by geometery at " CPS))
	 (setq pcount (+ pcount 1))
	 (setq cgpl (append cgpl (list CPS)(list "sideshot")(list "existing")(list (rtos pcount 2 0))))
         (setq CPn (rtos pcount 2 0))
	 	 ))
(if (> bf 0)(setq rot "ccw")(setq rot "cw"))
       		   (setq pflist (append pflist (list (strcat"      <Curve radius=\"" (rtos radius 2 6) "\" rot=\"" rot "\">"))))
		   (setq pflist (append pflist (list (strcat "        <Start pntRef=\"CGPNT-"p1n"\"/>"))))
                   (setq pflist (append pflist (list (strcat "        <Center pntRef=\"CGPNT-"cpn"\"/>"))))
		   (setq pflist (append pflist (list (strcat "        <End pntRef=\"CGPNT-"p2n"\"/>"))))
		   (setq pflist (append pflist (list "      </Curve>")))
       );p
     );if LINE OR ARC
  (setq count1 (+ count1 2))
   );r length of ptlist
  (setq pflist (append pflist (list "    </CoordGeom>")))
  (setq pflist (append pflist (list "  </PlanFeature>")))
  (setq count (+ count 1))
  (setq pffcount (+ pffcount 1))
);r length of lots
))

   ;14.Structural Boundaries - Polylines

    (princ "\nStructural Boundaries PolyLines, ")
 (IF (/= (setq lots (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Boundary Wall Interior,Boundary Wall Exterior,Boundary Wall Median,Boundary Wall Other")))) nil)(progn
(setq count 0)
  (repeat (sslength lots)
    (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))
    (SETQ layer (CDR(ASSOC 8 (ENTGET (SSNAME lots COUNT)))))
    
  
    (SETQ CLOSED (CDR(ASSOC 70 (ENTGET (SSNAME lots COUNT)))))
;write to list

     
    (if (= layer  "Boundary Wall Interior") (setq walltype "Interior Face"))
    (if (= layer  "Boundary Wall Exterior") (setq walltype "Exterior Face"))
    (if (= layer  "Boundary Wall Median") (setq walltype "Median"))
    (if (= layer  "Boundary Wall Other") (setq walltype "Other"))
    
    
 
					(setq enlist (entget en))
    ;go through polyline to get points and bugle factors
    (SETQ PTLIST (LIST))
	    (foreach a enlist
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	      (if (= 42 (car a))
		(setq PTLIST (append PTLIST (LIST (cdr a))))
		);if
	    )				;FOREACH 			
(IF (= CLOSED 1)(SETQ PTLIST (APPEND PTLIST (LIST(NTH 0 PTLIST))(LIST(NTH 1 PTLIST)))))

    (setq count1 0)
 (repeat (- (/ (length ptlist) 2) 1)
   (setq p1 (nth count1 ptlist))
   (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
   (setq p2 (nth (+ count1 2) ptlist))
   (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))
   (setq bf (nth (+ count1 1) ptlist))
   ;get edge names


   (setq buildbdys (append buildbdys (list (strcat p1s "-" p2s) walltype )(list (strcat p2s "-" p1s) walltype)))
   (setq buildpnts (append buildpnts (list p1s p2s)))

     
  (setq count1 (+ count1 2))
   );r length of ptlist
 
  (setq count (+ count 1))
 
);r length of lots
))



    

   ;12.Structural Boundaries - Lines
(princ "Lines, ")
  (IF (/= (setq bdyline (ssget "_X" '((0 . "LINE") (8 . "Boundary Wall Interior,Boundary Wall Exterior,Boundary Wall Median,Boundary Wall Other")))) nil)(progn 
  (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ LAYER (CDR(ASSOC 8 (ENTGET (SSNAME bdyline COUNT)))))

       (if (= layer  "Boundary Wall Interior") (setq walltype "Interior Face"))
    (if (= layer  "Boundary Wall Exterior") (setq walltype "Exterior Face"))
    (if (= layer  "Boundary Wall Median") (setq walltype "Median"))
    (if (= layer  "Boundary Wall Other") (setq walltype "Other"))
    
    
    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))

     (setq buildbdys (append buildbdys (list (strcat p1s "-" p2s) walltype )(list (strcat p2s "-" p1s) walltype)))
    (setq buildpnts (append buildpnts (list p1s p2s)))
    
	    (setq count (+ count 1))
    );r
  );p
     );if


     ;13.Structural Boundarys - Arcs
(princ "Arcs")
  (IF (/= (setq bdyline (ssget "_X" '((0 . "ARC") (8 . "Boundary Wall Interior,Boundary Wall Exterior,Boundary Wall Median,Boundary Wall Other")))) nil)(progn
 (setq count 0)
  (repeat (sslength bdyline)
(SETQ CP (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ RADIUS (CDR(ASSOC 40 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG1 (CDR(ASSOC 50 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG2 (CDR(ASSOC 51 (ENTGET (SSNAME bdyline COUNT)))))
  
  (SETQ P1 (POLAR CP ANG1 RADIUS))
  (SETQ P2 (POLAR CP ANG2 RADIUS))

   
    
    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))
    (setq cps (strcat (rtos (cadr cp) 2 4) " " (rtos (car cp) 2 4)))

           (if (= layer  "Boundary Wall Interior") (setq walltype "Interior Face"))
    (if (= layer  "Boundary Wall Exterior") (setq walltype "Exterior Face"))
    (if (= layer  "Boundary Wall Median") (setq walltype "Median"))
    (if (= layer  "Boundary Wall Other") (setq walltype "Other"))

     (setq buildbdys (append buildbdys (list (strcat p1s "-" p2s) walltype )(list (strcat p2s "-" p1s) walltype)))
  (setq buildpnts (append buildpnts (list p1s p2s)))
	  
	    (setq count (+ count 1))
    )));r


 
    
  
      ;15. Occupation lines
  (princ "\nProcessing Occupation Offest Lines")
(IF (/= (setq bdyline (ssget "_X" '((0 . "LINE") (8 . "Occupations")))) nil)(progn 
  (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	  (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Occupation Line with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
        (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))

    (if (= (setq remlist(member p1s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq cgpl (append cgpl (list p1s)(list "monument")(list "proposed")(list (rtos pcount 2 0))))
				   );p
       );&if
				   
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (setq pcount (+ pcount 1))
				   (setq cgpl (append cgpl (list p2s)(list "monument")(list "proposed")(list (rtos pcount 2 0))))
				   );p
       );&if
(if (/= (setq remlist (member p1s cgpl)) nil)(progn
						  (setq p1n (nth 3 remlist))
						  ))
         (if (/= (setq remlist (member p2s cgpl)) nil)(progn
						  (setq p2n (nth 3 remlist))
						  ))

    (setq xdatai (strcat "<PlanFeature name=\"OFF-" (rtos pffcount 2 0) "\" " xdatai))

    
    (setq pflist (append pflist (list xdatai)
			 (list (strcat "    <CoordGeom name=\"CG-"(rtos cogeocount 2 0) "\">"))
			 (list (strcat"      <Line>"))
			 (list (strcat "      <Start pntRef=\"CGPNT-"p1n"\"/>"))
			 (list (strcat "      <End pntRef=\"CGPNT-"p2n"\"/>"))
			 (list "      </Line>")
			 (list "    </CoordGeom>")
			 (list "  </PlanFeature>")
			 ))

    (setq pffcount (+ pffcount 1))
	    (setq cogeocount (+ cogeocount 1))
	    (setq count (+ count 1))
    )));r


     ;16.Chainages
    
    (princ "\nChainages")
 (IF (/= (setq lots (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Occupations")))) nil)(progn
(setq count 0)
  (repeat (sslength lots)
    (setq chainlist nil)
    (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))
    (SETQ layer (CDR(ASSOC 8 (ENTGET (SSNAME lots COUNT)))))
    
   (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (/= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL)
    (PROGN
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))))
        (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))
    (SETQ CLOSED (CDR(ASSOC 70 (ENTGET (SSNAME lots COUNT)))))

    ;CREATE LIST OF CHAINAGES
    ;chop out description if it exists
    (IF (= (SUBSTR XDATAI 1 2) "00")(SETQ pfdesc "Chainage")
      (progn
      (setq ,pos (vl-string-position 44 xdatai 0))
      (setq pfdesc (substr xdatai 1 ,pos))
      (setq xdatai (substr xdatai (+ ,pos 2) 200000))
      ))
    ;make list of chainages
    (while (/= (setq ,pos (vl-string-position 44 xdatai 0)) nil)
      (progn
	(setq chainlist (append chainlist (list (substr xdatai 1 ,pos))))
	(setq xdatai (substr xdatai (+ ,pos 2) 200000))
))
    (setq chainlist (append chainlist  (list xdatai)))
    ;write to list
  (setq pflist (append pflist (list (strcat"  <PlanFeature name=\"CHAIN-"  (rtos pffcount 2 0) "\" desc=\""pfdesc"\">"))))
  (setq pflist (append pflist (list (strcat "    <CoordGeom name=\"CG-" (rtos cogeocount 2 0) "\">"))))
  (setq cogeocount (+ cogeocount 1))
					(setq enlist (entget en))
    ;go through polyline to get points and bugle factors
    (SETQ PTLIST (LIST))
	    (foreach a enlist
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	      (if (= 42 (car a))
		(setq PTLIST (append PTLIST (LIST (cdr a))))
		);if
	    )				;FOREACH 			
(IF (= CLOSED 1)(SETQ PTLIST (APPEND PTLIST (LIST(NTH 0 PTLIST))(LIST(NTH 1 PTLIST)))))

    (setq count1 0);ptlist counter
    (setq count2 1);chainlist counter
 (repeat (- (/ (length ptlist) 2) 1)
   (setq p1 (nth count1 ptlist))
   (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
   (setq p2 (nth (+ count1 2) ptlist))
   (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))
   (setq bf (nth (+ count1 1) ptlist))
   ;get edge names
     (if (/= (setq remlist (member p1s cgpl)) nil)(progn
						  (setq p1n (nth 3 remlist))
						  )
       (progn
	 ;(princ (strcat"\n Lot corner not defined by geometery at " p1s))
         (setq pcount (+ pcount 1))
	 (setq cgpl (append cgpl (list p1s)(list "monument")(list "proposed")(list (rtos pcount 2 0))))
         (setq p1n (rtos pcount 2 0))
	 	 ))
     (if (/= (setq remlist (member p2s cgpl)) nil)(progn
						  (setq p2n (nth 3 remlist))
						  )
       (progn;if not found at corner
	 ;(princ (strcat "\n Lot corner yet not defined by geometery at " p2s))
	 (setq pcount (+ pcount 1))
	 (setq cgpl (append cgpl (list p2s)(list "monument")(list "proposed")(list (rtos pcount 2 0))))
         (setq p2n (rtos pcount 2 0))
	 	 ))
   (if (= bf 0.0)(progn ;is a line
		   (setq pflist (append pflist (list (strcat "      <Line desc=\"" (nth count2 chainlist) "\">"))))
		   (setq pflist (append pflist (list (strcat "        <Start pntRef=\"CGPNT-"p1n"\"/>"))))
		   (setq pflist (append pflist (list (strcat "        <End pntRef=\"CGPNT-"p2n"\"/>"))))
		   (setq pflist (append pflist (list "      </Line>")))
		   );p
     
     );if LINE OR ARC
  (setq count1 (+ count1 2))
   (setq count2 (+ count2 1))
   );r length of ptlist
  (setq pflist (append pflist (list "    </CoordGeom>")))
  (setq pflist (append pflist (list "  </PlanFeature>")))
  (setq count (+ count 1))
  (setq pffcount (+ pffcount 1))
);r length of lots
))


;17.---------------------------------------admin sheet-----------------------------

     (princ "\nProcessing Admin Sheet")

(IF (/= (setq adminsheet (ssget "_X" '((0 . "INSERT") (2 . "ADMINSHEET")))) nil)(progn
		(SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME ADMINSHEET 0)))))								

		(setq count 1)
  (SETQ ATTLIST (LIST))
	    
(setq Obj (vlax-ename->vla-object En))
	(foreach x (vlax-invoke Obj 'GetAttributes)
	  (SETQ ATT (vla-get-textstring X))
	  (if (= att "none") (setq att ""))

	  (setq &pos 0)
	   (while (/=  (setq &pos (vl-string-search "&" att &pos )) nil) (setq att (vl-string-subst "&amp;" "&"  att &pos)
										      &pos (+ &pos 4)))
	  ;(setq crlfpos 0)
	  ; (while (/=  (setq crlfpos (vl-string-search "\p" att crlfpos )) nil) (setq att (vl-string-subst "&#xA;" "\p"  att crlfpos)
	;									      crlfpos (+ crlfpos 5)))


	  (setq attlist (append attlist (list att)))

      
	  )
		(setq plannum (nth 0 attlist))
	  (if (and (> (ascii (substr plannum (strlen plannum) 1)) 64) (< (ascii (substr plannum (strlen plannum) 1)) 91))(setq plannum (substr plannum 1 (- (strlen plannum )1 ))))
	
	      
  )
  (princ "\nERROR No Admin Sheet Found")
  );P&IF ADMINSHEET EXISTS
    
    

    
;18.-----------Easement and Restriction linking "Retardo-Parcels"-----------

(princ "\nProcessing Restriction Descriptions and Annotations")

    (IF (/= (setq lots (ssget "_X" '((0 . "MTEXT") (8 . "Admin Sheet")))) nil)(progn
										
	(setq count 0)
  (repeat (sslength lots)
     (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))
    (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME lots COUNT)))))
      	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (/= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
		    
        ; (setq &pos 0)
	 ;     (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
	;									      &pos (+ &pos 4)))
	;	    (setq \pos 0)
	;	    (while (/=  (setq \pos (vl-string-search (strcat (chr 92)(chr 92)) xdatai \pos )) nil) (setq xdatai (vl-string-subst (chr 92) (strcat (chr 92)(chr 92))  xdatai \pos)
	;									      \pos (+ \pos 1)))
		    
 (setq mel (entget en))
 (setq txt "")
 (while (setq str (cdr (assoc 3 mel)))
   (progn
 (setq txt (strcat txt str))
 (setq mel (cdr (member (assoc 3 mel) mel)))
 ))
 (setq txt (strcat txt (cdr (assoc 1 (entget en)))))


		      ;replace & and linefeeds and characters
		    (setq &pos 0)
		    (while (/=  (setq &pos (vl-string-search "&" txt &pos )) nil) (setq txt (vl-string-subst "&amp;" "&"  txt &pos)
										      &pos (+ &pos 4)))
	  
	  (setq crlfpos 0)
	   (while (/=  (setq crlfpos (vl-string-search "\\P" txt crlfpos )) nil) (setq txt (vl-string-subst "&#xA;" "\\P"  txt crlfpos)
										      crlfpos (+ crlfpos 5)))
           (setq crlfpos 0)
	   (while (/=  (setq crlfpos (vl-string-search "\n" txt crlfpos )) nil) (setq txt (vl-string-subst "&#xA;" "\n"  txt crlfpos)
										      crlfpos (+ crlfpos 5)))

		    (setq sybpos 0)
	      (while  (/=  (setq sybpos (vl-string-search "'" txt sybpos )) nil)
		(setq txt (vl-string-subst "&apos;" "'"  txt sybpos)
		      sybpos (+ sybpos 4)))

		    (setq sybpos 0)
	      (while  (/=  (setq sybpos (vl-string-search (chr 34) txt sybpos )) nil)
		(setq txt (vl-string-subst "&quot;" (chr 34)  txt sybpos)
		      sybpos (+ sybpos 4)))

		    (setq sybpos 0)
		    (while (/=  (setq sybpos (vl-string-search "°" txt sybpos )) nil)
		(setq txt (vl-string-subst "&#176;" "°"  txt sybpos)
		      sybpos (+ sybpos 5)))



		    

		    (IF  (= (strcase (substr txt 1 29 )) "PARCEL WITH AREA BY DEDUCTION")(setq txt (substr txt 1 29)))

		    ;(princ (substr xdatai 1 33 ))
		    
		    (if (= (substr xdatai 1 11 ) "<Annotation")(progn
			(if (AND (/= (substr xdatai 1 33 ) "<Annotation type=\"Easement Width\"" )(vl-string-search "pclRef" xdatai)(= (vl-string-search "\\" xdatai ) nil))(setq xdatai (strcat (substr xdatai 1 (- (strlen xdatai)1)) "\\" plannum "\"")))
(setq anno (strcat xdatai  " desc=\"" txt "\" name=\"ANNO-" (rtos annocount 2 0) "\" />" (chr 10)))
			(setq annofront (strcat   xdatai  " desc=\"" txt "\" name=\"ANNO-"))
			(if (= (vl-string-search annofront annolist) nil)
			  (progn
			    (setq annolist (strcat annolist anno));store to anno list if not already in there eg 2 ease widths
			    (setq annocount (+ annocount 1))
			    ));p&if anno not in list


								 );p
		      (progn ;else store restriction
		    (if (= (vl-string-search "\\" xdatai ) nil)(setq xdatai (strcat xdatai "\\" plannum)))
 (setq resdesclist (append resdesclist (list xdatai)(list txt)))
));p&if annotation
 
 ));p&if mtext xdata
    
 (setq count (+ count 1))
 )
	));if mext
 
 
 
;19.-------------------------------------link parcels-----------------------------------------
    
(princ "\nProcessing Link Parcel Points")

(IF (/= (setq lots (ssget "_X" '((0 . "POINT") (8 . "Lot Definitions")))) nil)(progn

	(setq count 0)
  (repeat (sslength lots)
     (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))
    (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME lots COUNT)))))
      	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Link parcel with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
         (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))
(if (/= (setq stringpos (vl-string-search "name" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq pclname (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))))
    
    ;if easement seperate out link parcel
     (if (setq !pos1 (vl-string-position 33 xdatai 0))(progn
                      (setq easelink (substr xdatai (+ !pos1 2) 200))
                      (setq xdatai  (substr xdatai 1 !pos1))
    
(setq easelinklist (append easelinklist (list (strcat xdatai "!" easelink))))
		      
		      ))
    
(if (/= (vl-string-search "class=\"Restriction\"" xdatai ) nil)(progn
								  (if (= (vl-string-search "\\" pclname ) nil)
								    (progn
								      (setq pclname (strcat pclname "\\" plannum))
								      (setq xdatai (vl-string-subst (strcat "name=\"" pclname "\\" plannum) (strcat "name=\"" pclname) xdatai))
								      ));if created lot
																 
								 (setq reslinklist (append reslinklist (list pclname xdatai)))
								 ));if restriction

  
    

    ;if OC main
   (if (/= (vl-string-search "class=\"Owners Corporation\"" xdatai ) nil)(setq ocmain xdatai))
   (if (/= (vl-string-search "lotEntitlements=" xdatai ) nil)(setq oclist (append oclist (list xdatai))))
    
    
   
    (setq count (+ count 1))


    

    
    );r

    ))



;20.------------Created lots----------------------------

  
(princ "\nProcessing Created Lots")

(IF (/= (setq lots (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Lot Definitions")))) nil)(progn

	;check all lots for multipart lots
								     
			(setq count 0)
  (repeat (sslength lots)
     (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))
    (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME lots COUNT)))))
      	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Lot with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
         (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))
    
 (if (/= (setq stringpos (vl-string-search "parcelType" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 12)))(setq pcltype (substr xdatai (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11))))))
     (if (/= (setq stringpos (vl-string-search "name" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq pclname (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq pclname ""))
     (if (/= (setq stringpos (vl-string-search "class" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 7)))(setq pclclass (substr xdatai (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq lotclass ""))
(if (/= (setq stringpos (vl-string-search "state" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 7)))(setq pclstate (substr xdatai (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq pclstate ""))
(if (and (= (vl-string-search "\\" pclname ) nil)(/= pclclass "Easement")) (setq pclname (strcat pclname "\\" plannum)));add plan name
    
     (if (/= (setq stringpos (vl-string-search "area" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq area (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq area ""))
    
    (if (= pcltype "Part")(setq mplist (append mplist (list (strcat pclname "," area "," pclclass "," pclstate )))))
    (setq count (+ count 1))
    );r
(if (> (length mplist) 0)(progn
			  
		(setq mplist (vl-sort mplist '<))
			(setq count 1)
			(setq alphanum "1")
			(setq pclinfo (nth 0 mplist))

		 (setq ,pos1 (vl-string-position 44 pclinfo 0))
      (setq ,pos2 (vl-string-position 44 pclinfo (+ ,pos1 1)))
      (setq ,pos3 (vl-string-position 44 pclinfo (+ ,pos2 1)))
     
      (setq pclname     (substr pclinfo 1 ,pos1))
      (setq area        (substr pclinfo (+ ,pos1 2) (- (- ,pos2 ,pos1) 1)))
      (setq pclclass    (substr pclinfo (+ ,pos2 2) (- (- ,pos3 ,pos2) 1)))
      (setq pclstate    (substr pclinfo (+ ,pos3 2)))
		
			(setq areatot area)
                        (if (= pclclass "Easement")(setq parcelformat "parcelFormat=\"Geometry\" ")(setq parcelformat ""))
			(setq liststart (strcat "  <Parcel name=\""pclname "\" class=\"" pclclass "\" state=\""pclstate "\" parcelType=\"Multipart\" "  parcelformat "area=\""))
;add restriction description
		
			(setq listend (strcat ">" (chr 10) "    <Parcels>"  ))
		(if (= pclclass "Restriction")
			  (progn
			    (setq listend  (strcat " desc=\"" (cadr (member pclname resdesclist)) "\"" listend))
			  (setq resnth (vl-position pclname resdesclist))
	(setq resdesclist (remove_nth resdesclist resnth))
	(setq resdesclist (remove_nth resdesclist resnth ))
	))
		(setq linkno (+ linkno 1))
		(if (vl-string-search "\\" pclname)
		  (setq listend (strcat listend (chr 10)"      <Parcel name=\"LNK-" (rtos linkno 2 0) "\" pclRef=\"" (vl-string-subst (strcat  "-p" alphanum "\\") "\\" pclname ) "\" />"  ));if normal or restriction
      		        (setq listend (strcat listend (chr 10)"      <Parcel name=\"LNK-" (rtos linkno 2 0) "\" pclRef=\"" pclname "-p" alphanum "\"/>"  )) ;if easement
			);if slash
			(setq mpalist (append mpalist (list pclname)(list alphanum)))
			(setq prevname pclname)
			;sort list
		(setq count 1)
			(repeat (-  (length mplist) 1)
			  (setq pclinfo (nth count mplist))

		 (setq ,pos1 (vl-string-position 44 pclinfo 0))
      (setq ,pos2 (vl-string-position 44 pclinfo (+ ,pos1 1)))
      (setq ,pos3 (vl-string-position 44 pclinfo (+ ,pos2 1)))
     
      (setq pclname     (substr pclinfo 1 ,pos1))
      (setq area        (substr pclinfo (+ ,pos1 2) (- (- ,pos2 ,pos1) 1)))
      (setq pclclass    (substr pclinfo (+ ,pos2 2) (- (- ,pos3 ,pos2) 1)))
      (setq pclstate    (substr pclinfo (+ ,pos3 2)))
			  
			  
			  (if (= pclname prevname)(progn
			    (setq alphanum (rtos  (+ (atof alphanum) 1)))
			    (setq linkno (+ linkno 1))
			    (if (vl-string-search "\\" pclname)
		  (setq listend (strcat listend (chr 10)"      <Parcel name=\"LNK-" (rtos linkno 2 0) "\" pclRef=\"" (vl-string-subst (strcat  "-p" alphanum "\\") "\\" pclname ) "\" />"  ));if normal or restriction
      		        (setq listend (strcat listend (chr 10)"      <Parcel name=\"LNK-" (rtos linkno 2 0) "\" pclRef=\"" pclname "-p" alphanum "\"/>"  )) ;if easement
			);if slash
			    
			    (setq areatot (rtos (+ (atof area) (atof areatot))))
)
			    (progn;else not the same pclname
			      (setq mpolist (append mpolist (list (strcat liststart areatot "\""  listend (chr 10) "    </Parcels>" (chr 10)   "  </Parcel>"))))
			      
			      (setq alphanum "1")
			      (setq areatot area)
			      (if (= pclclass "Easement")(setq parcelformat "parcelFormat=\"Geometry\" ")(setq parcelformat ""))
			      (setq liststart (strcat "  <Parcel name=\""pclname "\" class=\"" pclclass "\" state=\""pclstate "\" parcelType=\"Multipart\" " parcelformat "area=\""))
			      
			      (setq listend (strcat ">" (chr 10) "    <Parcels>"  ))
			      
			      	(if (= pclclass "Restriction")
			  (progn
			  (setq listend  (strcat " desc=\"" (cadr (member pclname resdesclist)) "\"" listend))
			  (setq resnth (vl-position pclname resdesclist))
			    (setq resdesclist (remove_nth resdesclist resnth))
	(setq resdesclist (remove_nth resdesclist resnth ))
	))
			      
			(if (vl-string-search "\\" pclname)
		  (setq listend (strcat listend (chr 10)"      <Parcel name=\"LNK-" (rtos linkno 2 0) "\" pclRef=\"" (vl-string-subst (strcat  "-p" alphanum "\\") "\\" pclname ) "\" />"  ));if normal or restriction
      		        (setq listend (strcat listend (chr 10)"      <Parcel name=\"LNK-" (rtos linkno 2 0) "\" pclRef=\"" pclname "-p" alphanum "\"/>"  )) ;if easement
			);if slash
			    )
			    );if
			  
			  (setq mpalist (append mpalist (list pclname)(list  alphanum)))
			  (setq prevname pclname)
			  (setq count (+ count 1))
			  );r

			(setq mpolist (append mpolist (list (strcat liststart areatot "\"" listend  (chr 10) "    </Parcels>" (chr 10)   "  </Parcel>"))))
			));p&if multipart lots exist


;start reading lots
			
			;START 
 (setq count 0)
  (repeat (sslength lots)
  
    (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME lots COUNT)))))
    (SETQ CLOSED (CDR(ASSOC 70 (ENTGET (SSNAME lots COUNT))))) ;CLOSED FLAG
    

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (SETQ XDATAI (ASSOC -3 XDATAI))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))




    
(if (= (substr xdatai 1 4) "desc")(progn

				    ;Check for easement or road lots note an imported road lot will not have "desc" as the first four letters and not need assignment as below
    (if (/= (setq stringpos (vl-string-search "class" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 7)))(setq lotclass (substr xdatai (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq lotclass ""))
        (if (/= (setq stringpos (vl-string-search "parcelType" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 12)))(setq pcltype (substr xdatai (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11)))))(setq pcltype ""))
     (if (= lotclass "Road")(progn
			      (if (= pclstate "created")(setq xdatai (strcat "  <Parcel name=\"R" (rtos roadcount 2 0) "\"" (nth 0 attlist) " " xdatai)))
			      (if (= pclstate "existing") (setq xdatai (strcat "  <Parcel name=\"ROAD-" (rtos roadcount 2 0) "\""  " " xdatai)))
			     (setq roadcount (+ roadcount 1))
			     )
      )
    (if (= lotclass "Easement")(progn
				 (setq xdatai (strcat "  <Parcel name=\"E" (rtos easecounta 2 0) "\" " xdatai))
				 (setq easecounta (+ easecounta 1))
				 )
      )
    ));p& if desc

  
    

   
     
    
        (if (/= (setq stringpos (vl-string-search "class" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 7)))(setq pclclass (substr xdatai (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq lotclass ""))
     (if (/= (setq stringpos (vl-string-search "parcelType" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 12)))(setq pcltype (substr xdatai (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11)))))(setq pcltype ""))

    (if (/= (setq stringpos (vl-string-search "name" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq pclname (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))))
    
    (if  (and (/= (substr pclname 1 2) "PC") (= (vl-string-search "\\" pclname ) nil)(/= pclclass "Easement"))
      (progn
	(setq xdatai (vl-string-subst (strcat "name=\"" pclname "\\" plannum) (strcat "name=\"" pclname) xdatai))
	(setq pclname (strcat pclname "\\" plannum))
	))
    
     ;check for mutliplart lot
    (if (/= (setq listpos (vl-position pclname mpalist)) nil)(progn
							    (setq alpha (nth (+ listpos 1) mpalist))
							    (if (vl-string-search "\\" pclname)
							      (setq pclpartname (vl-string-subst (strcat "-p" alpha "\\") "\\" pclname));if normal or restriction part lot
							      (setq pclpartname (strcat pclname "-p" alpha));else easement part
							      );if slash
							    (setq mpalist (remove_nth mpalist (+ listpos 1)))
							    (setq mpalist (remove_nth mpalist listpos ))
							    (setq xdatai (vl-string-subst (strcat "name=\"" pclpartname)   (strcat "name=\"" pclname) xdatai))
							    )
      )


          
;get pcl state so exsiting lots dont get checked for geometery      
(if (/= (setq stringpos (vl-string-search "state" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 7)))(setq pclstate (substr xdatai (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq pclstate ""))


  

;seperate centrepoint coord out.
    
     (setq !pos1 (vl-string-position 33 xdatai 0))
                      (setq lotc (substr xdatai (+ !pos1 2) 200))
                      (setq xdatai  (substr xdatai 1 !pos1))
  

    (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))
    
  
  
(setq pcount (+ pcount 1))
  (setq cgpl (append cgpl (list lotc)(list "sideshot")(list "existing")(list (rtos pcount 2 0))))

    ;look for titles
 
    (setq titles (list))

      (while (/= (setq stringpos (vl-string-search "<Title" xdatai )) nil)(progn
(setq wwpos (vl-string-position 62 xdatai (+ stringpos 6)))(setq stringpos1  stringpos
								  stringpos2 (+ wwpos 2) )
    
   
  
    
(setq xdatafront (substr xdatai 1   stringpos1  ))
    (setq xdataback (substr xdatai stringpos2 ))
    (setq title  (substr xdatai (+ stringpos1 1) (- stringpos2 stringpos1 1)))
     (if (/= (setq stringpos (vl-string-search "name" title )) nil)(progn
(setq wwpos (vl-string-position 34 title (+ stringpos 6)))(setq titlename (substr title (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))))
    (if (and (= (vl-string-search "\\" titlename ) nil) (= (vl-string-search "/" titlename ) nil))
      (progn
	(setq title (vl-string-subst (strcat "name=\"" titlename "\\" plannum) (strcat "name=\"" titlename) title))
	))

    (setq titles (append titles (list title)))
    (setq xdatai (strcat xdatafront xdataback))
    ));p&w title

    

    
;if there is a small address seperate out and make it big
    (setq largeaddress "")
    (if (/= (setq stringpos (vl-string-search "smalladdress" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 14)))(setq smalladdress (substr xdatai (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq smalladdress ""))


    (if (/= smalladdress "")(progn

			      (setq xdatai (strcat (substr xdatai 1 (- stringpos 1))(substr xdatai (+ wwpos 4))))

			      
      (setq ,pos1 (vl-string-position 44 smalladdress 0))
      (setq ,pos2 (vl-string-position 44 smalladdress (+ ,pos1 1)))
      (setq ,pos3 (vl-string-position 44 smalladdress (+ ,pos2 1)))
      (setq ,pos4 (vl-string-position 44 smalladdress (+ ,pos3 1)))
      (setq ,pos5 (vl-string-position 44 smalladdress (+ ,pos4 1)))
      (setq ,pos6 (vl-string-position 44 smalladdress (+ ,pos5 1)))
      (setq ,pos7 (vl-string-position 44 smalladdress (+ ,pos6 1)))
      (setq ,pos8 (vl-string-position 44 smalladdress (+ ,pos7 1)))
      (setq ,pos9 (vl-string-position 44 smalladdress (+ ,pos8 1)))
      (setq ,pos10 (vl-string-position 44 smalladdress (+ ,pos9 1)))
      (setq ,pos11 (vl-string-position 44 smalladdress (+ ,pos10 1)))
      (setq ,pos12 (vl-string-position 44 smalladdress (+ ,pos11 1)))
      (setq ,pos13 (vl-string-position 44 smalladdress (+ ,pos12 1)))

      (setq flatNumber  (substr smalladdress 1 ,pos1))
      (setq flatType         (substr smalladdress (+ ,pos1 2) (- (- ,pos2 ,pos1) 1)))
      (setq floorLevelNumber (substr smalladdress (+ ,pos2 2) (- (- ,pos3 ,pos2) 1)))
      (setq floorLevelType   (substr smalladdress (+ ,pos3 2) (- (- ,pos4 ,pos3) 1)))
      (setq numberfirst      (substr smalladdress (+ ,pos4 2) (- (- ,pos5 ,pos4) 1)))
      (setq numberSuffixFirst(substr smalladdress (+ ,pos5 2) (- (- ,pos6 ,pos5) 1)))
      (setq numberLast       (substr smalladdress (+ ,pos6 2) (- (- ,pos7 ,pos6) 1)))
      (setq numberSuffixLast (substr smalladdress (+ ,pos7 2) (- (- ,pos8 ,pos7) 1)))
      (setq roadName         (substr smalladdress (+ ,pos8 2) (- (- ,pos9 ,pos8) 1)))
      (setq roadnamesuffix   (substr smalladdress (+ ,pos9 2) (- (- ,pos10 ,pos9) 1)))
      (setq roadnametype     (substr smalladdress (+ ,pos10 2) (- (- ,pos11 ,pos10) 1)))
      (setq roadtype         (substr smalladdress (+ ,pos11 2) (- (- ,pos12 ,pos11) 1)))
      (setq adminareaname    (substr smalladdress (+ ,pos12 2) (- (- ,pos13 ,pos12) 1)))
      (setq adminAreaCode    (substr smalladdress (+ ,pos13 2)))

      (if (/= flatnumber "")(setq flatnumber (strcat "flatNumber=\"" flatnumber "\" ")))
      (if (/= flatType "")(setq flatType (strcat "flatType=\"" flatType "\" ")))
      (if (/= floorLevelNumber "")(setq floorLevelNumber (strcat "floorLevelNumber=\""floorLevelNumber  "\" ")))
      (if (/= floorLevelType "")(setq floorLevelType (strcat "floorLevelType=\""floorLevelType  "\" ")))
      (if (/= numberfirst "")(setq numberfirst (strcat "numberFirst=\""numberfirst  "\" ")))
      (if (/= numberSuffixFirst "")(setq numberSuffixFirst (strcat "numberSuffixFirst=\""numberSuffixFirst  "\" ")))
      (if (/= numberLast  "")(setq numberLast  (strcat "numberLast=\""numberLast   "\" ")))
      (if (/= numberSuffixLast "")(setq numberSuffixLast (strcat "numberSuffixLast=\""numberSuffixLast  "\" ")))
      (if (/= roadName "")(setq roadName (strcat "roadName=\""roadName  "\" ")))
      (if (/= roadnamesuffix "")(setq roadnamesuffix (strcat "roadNameSuffix=\""roadnamesuffix "\" ")))
      (if (/= roadnametype "")(setq roadnametype (strcat "roadNameType=\""roadnametype  "\" ")))
      (if (/= roadtype "")(setq roadtype (strcat "roadType=\""roadtype  "\" ")))
      (if (/= adminareaname "")(setq adminareaname (strcat "adminAreaName=\""adminareaname  "\" ")))
      (if (/= adminAreaCode "")(setq adminAreaCode (strcat "adminAreaCode=\""adminAreaCode  "\" ")))

      (setq largeaddress (strcat "<LocationAddress addressType=\"Primary\" " flatnumber flattype floorlevelnumber floorleveltype numberfirst numbersuffixfirst numberlast numbersuffixlast ">" (chr 10)
				 "<RoadName " roadname roadnamesuffix roadnametype roadtype "/>" (chr 10)
				 "<AdministrativeArea " adminareaname "adminAreaType=\"Locality\" " adminareacode "/>" (chr 10)
				 "</LocationAddress>"))
      )
      (setq largeaddress "")
		)

    ;add restriction description
       
    
   (if (and (= pclclass "Restriction")(= pcltype "Single" ))
     (progn
       (setq xdatai (vl-string-subst (strcat " desc=\"" (cadr (member pclname resdesclist)) "\">") ">" xdatai))
;remove desc from list
       (setq resnth (vl-position pclname resdesclist))
	(setq resdesclist (remove_nth resdesclist resnth))
	(setq resdesclist (remove_nth resdesclist resnth ))
	)
      )
 

     
;write to list

  
  (setq lotlist (append lotlist (list xdatai)))
  (setq lotlist (append lotlist (list (strcat "    <Center pntRef=\"CGPNT-" (rtos pcount 2 0)"\"/>"))))
  (setq lotlist (append lotlist (list (strcat "    <CoordGeom name=\"CG-" (rtos cogeocount 2 0) "\">"))))
  (setq cogeocount (+ cogeocount 1))



  
					(setq enlist (entget en))
    ;go through polyline to get points to check for clockwise direction
    (SETQ CWLIST (LIST))
	    (foreach a enlist
	      (if (= 10 (car a))

		(setq CWLIST (append CWLIST (list (cdr a))))
	      )				;IF
	      
	    )				;FOREACH 			

  (IF (> (lcw CWLIST) 0) (command "pedit" en "r" "" ))
  

    

					(setq enlist (entget en))
    ;go through polyline to get points and bugle factors
    (SETQ PTLIST (LIST))
	    (foreach a enlist
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	      (if (= 42 (car a))
		(setq PTLIST (append PTLIST (LIST (cdr a))))
		);if
	    )				;FOREACH 			
 


    ;shuffle the list unitl one of the irregular line point isnt at the start
    (while (member (strcat (rtos (cadr (nth 0 ptlist)) 2 4) " " (rtos (car (nth 0 ptlist)) 2 4)) ibllist)
      (progn
	(setq minilist (list (nth 0 ptlist)(nth 1 ptlist)))
	(setq ptlist (remove_nth ptlist 0))
	(setq ptlist (remove_nth ptlist 0))
	(setq ptlist (append ptlist minilist))
	)
      )

    (IF (OR (= CLOSED 1)(= CLOSED 129)) (setq ptlist (append ptlist (list(nth 0 ptlist))(list (nth 1 ptlist)))))
    
	

 (setq count1 0)
    (setq repeater (/ (length ptlist) 2))
 (while (/= repeater 1)
   (progn

   (setq p1 (nth count1 ptlist))
   (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
   (setq p2 (nth (+ count1 2) ptlist))
   (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))
   (setq bf (nth (+ count1 1) ptlist))

   ;look for irregular line
   (if (/= (setq remiblselist (member (strcat p1s "-" p2s) iblselist)) nil)
     (progn

      
       (setq xdatai (nth 1 remiblselist))
        (setq lotlist (append lotlist (list (strcat "      <IrregularLine desc=\"" xdatai "\">"))))
       (setq irbl "")
       (setq pps "")
       (setq iblcount 0)
       (while (/= pps (nth 2 remiblselist))
	 (progn
	 (setq p (nth count1 ptlist))
   (setq ps (strcat (rtos (cadr p) 2 4) " " (rtos (car p) 2 4) " "))
	 (if (/= ps pps)(setq irbl (strcat irbl ps)))
	 (setq pps ps)
	 (setq count1 (+ count1 2))
	 (setq iblcount (+ iblcount 1))
	 ));p&w
       
     (setq ep (strcat (rtos (cadr p) 2 4) " " (rtos (car p) 2 4)))
     
      (if (/= (setq remlist (member p1s cgpl)) nil)(progn
						  (setq p1n (nth 3 remlist))
						  )
       (progn
	 (if (= pclstate "created")(progn
				     (if (= (member p1s buildpnts) nil)(progn
				     (princ (strcat"\nERROR Lot corner not defined by geometery at " (rtos (car p1) 2 6) "," (rtos (cadr p1) 2 6) " orange point placed"))
	 (command "color" "30" "point" p1 "color" "bylayer")
				     ))
	 ))
	 (setq pcount (+ pcount 1))
	 	 (setq cgpl (append cgpl (list p1s)(list "sideshot")(list "proposed")(list (rtos pcount 2 0))))
         (setq p1n (rtos pcount 2 0))
	 
	 	 ))
     (if  (/= (setq remlist (member ep cgpl)) nil)(progn
						  (setq epn (nth 3 remlist))
						  )
       (progn
	 (if (= pclstate "created")(progn
				     (if (= (member p1s buildpnts) nil)(progn
	 (princ (strcat"\nERROR Lot corner not defined by geometery at " (rtos (car p) 2 6) "," (rtos (cadr p) 2 6) " orange point placed"))
	 (command "color" "30" "point" p "color" "bylayer")
	 ))
	 ))
	 (setq pcount (+ pcount 1))
	 	 (setq cgpl (append cgpl (list ep)(list "sideshot")(list "proposed")(list (rtos pcount 2 0))))
         (setq epn (rtos pcount 2 0))
	 
	 	 ))

       (setq irbl (substr irbl 1 (- (strlen irbl) 1)))
     		   (setq lotlist (append lotlist (list (strcat "        <Start pntRef=\"CGPNT-"p1n"\"/>"))))
		   (setq lotlist (append lotlist (list (strcat "        <End pntRef=\"CGPNT-"epn"\"/>"))))
		(setq lotlist (append lotlist (list (strcat "        <PntList2D>" irbl "</PntList2D>"))))
             (setq lotlist (append lotlist (list "      </IrregularLine>")))
     (setq repeater (- repeater (- iblcount 1)))
       (setq count1 (- count1 2))
   
       )
     (progn;else

   ;get edge names
     (if (/= (setq remlist (member p1s cgpl)) nil)(progn
						  (setq p1n (nth 3 remlist))
						  )
       (progn
	 (if (= pclstate "created")(progn
				     (if (= (member p1s buildpnts) nil)(progn
	 (princ (strcat"\nERROR Lot corner not defined by geometery at " (rtos (car p1) 2 6) "," (rtos (cadr p1) 2 6) " orange point placed"))
	 (command "color" "30" "point" p1 "color" "bylayer")
	 ));if not a structural corner
				     ));if created
				     
	 
	 (setq pcount (+ pcount 1))
	 	 (setq cgpl (append cgpl (list p1s)(list "sideshot")(list "existing")(list (rtos pcount 2 0))))
         (setq p1n (rtos pcount 2 0))
	 
	 	 ))
     (if  (/= (setq remlist (member p2s cgpl)) nil)(progn
						  (setq p2n (nth 3 remlist))
						  )
       (progn;if not found at corner
	 (if (= pclstate "created")(progn
				     (if (= (member p1s buildpnts) nil)(progn
	 (princ (strcat "\nERROR Lot corner not defined by geometery at" (rtos (car p2) 2 6) "," (rtos (cadr p2) 2 6)" orange point placed"))
	 (command "color" "30" "point" p2 "color" "bylayer")
	 ));if not structural corner
				     ));if created
	 (setq pcount (+ pcount 1))
	 (setq cgpl (append cgpl (list p2s)(list "sideshot")(list "existing")(list (rtos pcount 2 0))))
         (setq p2n (rtos pcount 2 0))
	 
	 	 ))
(if (/= p1n p2n)(progn ;is not the same point

		  (if (and (= pclstate "created")(= (member (strcat p1n "-" p2n) obslist) nil)(=  (member (strcat p1s "-" p2s) buildbdys) nil) (=  (member (strcat p2s "-" p1s) buildbdys) nil))
		    (progn ;check if lot line has edge observation
		      (princ (strcat "\nLot edge not defined by observation from " (rtos (car p1) 2 3) "," (rtos (cadr p1) 2 3) " to " (rtos (car p2) 2 3) "," (rtos (cadr p2) 2 3) " orange line placed, suggest checking"))
			(command "color" "30" "line" p1 p2"" "color" "bylayer")
		      ));p & if no observation

		   (if (or (setq walltype  (cadr (member (strcat p1s "-" p2s) buildbdys))) (setq walltype  (cadr (member (strcat p2s "-" p1s) buildbdys))))
		    (progn ;check if lot line is a structural boundary
		      (setq linedesc (strcat " desc=\"" walltype "\" "))
		      );p
		     (progn ;else linedesc is nothing
		       (setq linedesc "")
		       );p
		     );if wall in buildbdys
		     
		  
   (if  (= bf 0.0)(progn ;is a line
		   (setq lotlist (append lotlist (list (strcat"      <Line" linedesc ">"))))
		   (setq lotlist (append lotlist (list (strcat "        <Start pntRef=\"CGPNT-"p1n"\"/>"))))
		   (setq lotlist (append lotlist (list (strcat "        <End pntRef=\"CGPNT-"p2n"\"/>"))))
		   (setq lotlist (append lotlist (list "      </Line>")))
		   );p
     (progn;is an arc

       
       ;FIGURE OUT centrePOINT
       
               (setq DI (DISTANCE P1 P2))
               (SETQ AZ (ANGLE P1 P2))
                (SETQ MP (POLAR P1 AZ (/ DI 2)))
       		(SETQ H (/ (* BF DI) 2))
		(SETQ AMP (POLAR MP (+ AZ (* 1.5 PI)) H))
		(SETQ X1 (CAR P1))
		(SETQ Y1 (CADR P1))
		(SETQ X2 (CAR AMP))
		(SETQ Y2 (CADR AMP))
		(SETQ X3 (CAR P2))
		(SETQ Y3 (CADR P2))
		(SETQ MA (/ (- Y2 Y1) (- X2 X1)))
		(SETQ MB (/ (- Y3 Y2) (- X3 X2)))
		(SETQ  RPX (/ (- (+ (* MA MB (- Y1 Y3)) (* MB (+ X1 X2)))  (* MA (+ X2 X3)) )(* 2 (- MB MA))))
		(SETQ  RPY (+ (* (* (/ 1 MA) -1) (- RPX (/ (+ X1 X2) 2)))  (/ (+ Y1 Y2) 2)))
       (setq radius (distance amp (list rpx rpy)))
       (SETQ CP (LIST RPX RPY))
		(SETQ CPS (strcat (rtos (cadr cp) 2 4) " " (rtos (car cp) 2 4)))

    
;note all that work to create the centerpoint should be redundant, as there should be an observation over this arc so....
       (if (or (setq remlist (member (strcat p1n "-" p2n) arclist))(setq remlist (member (strcat p2n "-" p1n) arclist)))
  (progn;is proposed and found in arc list
    (setq cpn (cadr remlist))
    )
  (progn;is not
    (if (and (= pclstate "created")(=  (member (strcat p1s "-" p2s) buildbdys) nil) (=  (member (strcat p2s "-" p1s) buildbdys) nil))(progn
    (princ (strcat "\nERROR Arc not defined by observation from " (rtos (car p1) 2 3) "," (rtos (cadr p1) 2 3) " to " (rtos (car p2) 2 3) "," (rtos (cadr p2) 2 3) " orange line placed, suggest checking"))
	(command "color" "30" "line" p1 p2"" "color" "bylayer")
    ))
    

       ;GET centrePPOINT NUMBER if already created
(if (/= (setq remlist (member CPS cgpl)) nil)(progn
						  (setq CPn (nth 3 remlist))
						  )
  
      (progn;else make new point
	 ;(princ (strcat "\n Arc centre not yet defined by geometery at " CPS))
	 (setq pcount (+ pcount 1))
	 (setq cgpl (append cgpl (list CPS)(list "sideshot")(list "existing")(list (rtos pcount 2 0))))
         (setq CPn (rtos pcount 2 0))
	 
	 	 ))

    
       	)
	 );if in arclist
       
(if (> bf 0)(setq rot "ccw")(setq rot "cw"))
       
       		   (setq lotlist (append lotlist (list (strcat"      <Curve radius=\"" (rtos radius 2 6) "\" rot=\"" rot "\"" linedesc ">"))))
		   (setq lotlist (append lotlist (list (strcat "        <Start pntRef=\"CGPNT-"p1n"\"/>"))))
                   (setq lotlist (append lotlist (list (strcat "        <Center pntRef=\"CGPNT-"cpn"\"/>"))))
		   (setq lotlist (append lotlist (list (strcat "        <End pntRef=\"CGPNT-"p2n"\"/>"))))
		   (setq lotlist (append lotlist (list "      </Curve>")))


       );p
     );if LINE OR ARC
		 
		  
		  
   ));p&if p1n is not p2n

       (setq count1 (+ count1 2))
      (setq repeater (- repeater 1))
     
     ));end else and if not irregular
     

    

     
   ));while repeater length of ptlist
  (setq lotlist (append lotlist (list "    </CoordGeom>")))
    (setq tcount 0)
    (repeat (length titles)
      (setq lotlist (append lotlist (list (nth tcount titles))))
      (setq tcount (+ tcount 1))
      )
    (if (/= largeaddress "")(setq lotlist (append lotlist (list largeaddress))))
    (setq lotlist (append lotlist (list "  </Parcel>")))


    
  (setq count (+ count 1))
);r length of lots
);p
  (PRINC "\nNo Lots found in Project")
  );if
 
  ;if lots found

  ;---------------------------------------------------easement and restriction links------------------------------------------------------------

  (PRINC "\nWriting easement links")

  (if (> (LENGTH easelinklist) 0)(progn
  
  (setq easelinklist (vl-sort easelinklist '<))
(setq count 0)
   (repeat  (length easelinklist)
  (setq easeinfo (nth count easelinklist))

  (if (/= (setq stringpos (vl-string-search "class" easeinfo )) nil)(progn
(setq wwpos (vl-string-position 34 easeinfo (+ stringpos 7)))(setq pclclass (substr easeinfo (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6))))))
   (if (/= (setq stringpos (vl-string-search "name" easeinfo )) nil)(progn
(setq wwpos (vl-string-position 34 easeinfo (+ stringpos 6)))(setq pclname (substr easeinfo (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))))

  ;(if (= (vl-string-search "\\" pclname) nil)(setq easeinfo (vl-string-subst (strcat "name=\"" pclname "\\" plannum)(strcat "name=\"" pclname) easeinfo)))

 
     
  (setq !pos1 (vl-string-position 33 easeinfo 0))
                      (setq subparcels (substr easeinfo (+ !pos1 2)))
                      (setq parcel  (substr easeinfo 1 !pos1))
     
     (setq lotlist (append lotlist (list parcel)))
  (setq lotlist (append lotlist (list "    <Parcels>")))

;write multiple parcels
     
  (while (/= (setq ,pos (vl-string-position 44 subparcels 1)) nil)
    (progn
    
    (setq subparcel (substr subparcels 1  ,pos ))
    (setq subparcels (substr subparcels (+ ,pos 2)))
  
  (setq lotlist (append lotlist (list (strcat "      <Parcel name=\"LNK-" (rtos linkno 2 0) "\" pclRef=\"" subparcel "\"/>"))))
    
  (setq linkno (+ linkno 1))
  
    ))

     ;last or only parcel
   (setq subparcel subparcels)
  (setq lotlist (append lotlist (list (strcat "      <Parcel name=\"LNK-" (rtos linkno 2 0) "\" pclRef=\"" subparcel "\"/>"))))
  (setq linkno (+ linkno 1))
  (setq prev,pos ,pos)
    
  
    (setq lotlist (append lotlist (list "    </Parcels>"    "  </Parcel>")))
   
    (setq count (+ count 1))
    );r

 
    
	));if easelinklist exists

;if there are any non spatial lots

  (PRINC "\nNon spatial lots/restrictions")

    ;non spatial restrictions
    (if (/= resdesclist nil)
      (progn
    (setq rescount 0)
(repeat (/ (length resdesclist) 2)

  (setq pclname (nth rescount resdesclist))
  
  (setq desc (nth (+ 1 rescount) resdesclist))

  (setq parcel (cadr (member pclname reslinklist)))
  
 

  (setq parcel (vl-string-subst (strcat " desc=\"" desc "\" >") ">" parcel))

  (if (/= (setq stringpos (vl-string-search "name" parcel )) nil)(progn
(setq wwpos (vl-string-position 34 parcel (+ stringpos 6)))(setq pclname (substr parcel (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))))
    (if (= (vl-string-search "\\" pclname ) nil)
      (progn
	(setq parcel (vl-string-subst (strcat "name=\"" pclname "\\" plannum) (strcat "name=\"" pclname) parcel))
	(setq pclname (strcat pclname "\\" plannum))
	))

  

  (setq lotlist (append lotlist  (list (strcat parcel ))))
  (setq lotlist (append lotlist (list "</Parcel>")))
  (setq rescount (+ rescount 1))

  );r

    ));if restrictions

    (PRINC "\nDepth Limitations")

    ;depth limitation
    (if (/= (setq dl (nth 12 attlist)) "")
      (progn
      
(setq lotlist (append lotlist  (list (strcat "<Parcel name=\"DL1\" class=\"Depth Limitation\" state=\"existing\" parcelType=\"Single\" desc=\"" dl "\" />"))))

      ))
    
    (if (/= ocmain nil)(progn

			 (PRINC "\nWriting Owners Corp links")

			 ;STRIP SMALL ADDRESS 

;if there is a small address seperate out and make it big
    (if (/= (setq stringpos (vl-string-search "smalladdress" ocmain )) nil)(progn
(setq wwpos (vl-string-position 34 ocmain (+ stringpos 14)))(setq smalladdress (substr ocmain (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq smalladdress ""))


    (if (/= smalladdress "")(progn

			      (setq ocmain (strcat (substr ocmain 1 (- stringpos 1))(substr ocmain (+ wwpos 4))))

			      
      (setq ,pos1 (vl-string-position 44 smalladdress 0))
      (setq ,pos2 (vl-string-position 44 smalladdress (+ ,pos1 1)))
      (setq ,pos3 (vl-string-position 44 smalladdress (+ ,pos2 1)))
      (setq ,pos4 (vl-string-position 44 smalladdress (+ ,pos3 1)))
      (setq ,pos5 (vl-string-position 44 smalladdress (+ ,pos4 1)))
      (setq ,pos6 (vl-string-position 44 smalladdress (+ ,pos5 1)))
      (setq ,pos7 (vl-string-position 44 smalladdress (+ ,pos6 1)))
      (setq ,pos8 (vl-string-position 44 smalladdress (+ ,pos7 1)))
      (setq ,pos9 (vl-string-position 44 smalladdress (+ ,pos8 1)))
      (setq ,pos10 (vl-string-position 44 smalladdress (+ ,pos9 1)))
      (setq ,pos11 (vl-string-position 44 smalladdress (+ ,pos10 1)))
      (setq ,pos12 (vl-string-position 44 smalladdress (+ ,pos11 1)))
      (setq ,pos13 (vl-string-position 44 smalladdress (+ ,pos12 1)))

      (setq flatNumber  (substr smalladdress 1 ,pos1))
      (setq flatType         (substr smalladdress (+ ,pos1 2) (- (- ,pos2 ,pos1) 1)))
      (setq floorLevelNumber (substr smalladdress (+ ,pos2 2) (- (- ,pos3 ,pos2) 1)))
      (setq floorLevelType   (substr smalladdress (+ ,pos3 2) (- (- ,pos4 ,pos3) 1)))
      (setq numberfirst      (substr smalladdress (+ ,pos4 2) (- (- ,pos5 ,pos4) 1)))
      (setq numberSuffixFirst(substr smalladdress (+ ,pos5 2) (- (- ,pos6 ,pos5) 1)))
      (setq numberLast       (substr smalladdress (+ ,pos6 2) (- (- ,pos7 ,pos6) 1)))
      (setq numberSuffixLast (substr smalladdress (+ ,pos7 2) (- (- ,pos8 ,pos7) 1)))
      (setq roadName         (substr smalladdress (+ ,pos8 2) (- (- ,pos9 ,pos8) 1)))
      (setq roadnamesuffix   (substr smalladdress (+ ,pos9 2) (- (- ,pos10 ,pos9) 1)))
      (setq roadnametype     (substr smalladdress (+ ,pos10 2) (- (- ,pos11 ,pos10) 1)))
      (setq roadtype         (substr smalladdress (+ ,pos11 2) (- (- ,pos12 ,pos11) 1)))
      (setq adminareaname    (substr smalladdress (+ ,pos12 2) (- (- ,pos13 ,pos12) 1)))
      (setq adminAreaCode    (substr smalladdress (+ ,pos13 2)))

      (if (/= flatnumber "")(setq flatnumber (strcat "flatNumber=\"" flatnumber "\" ")))
      (if (/= flatType "")(setq flatType (strcat "flatType=\"" flatType "\" ")))
      (if (/= floorLevelNumber "")(setq floorLevelNumber (strcat "floorLevelNumber=\""floorLevelNumber  "\" ")))
      (if (/= floorLevelType "")(setq floorLevelType (strcat "floorLevelType=\""floorLevelType  "\" ")))
      (if (/= numberfirst "")(setq numberfirst (strcat "numberFirst=\""numberfirst  "\" ")))
      (if (/= numberSuffixFirst "")(setq numberSuffixFirst (strcat "numberSuffixFirst=\""numberSuffixFirst  "\" ")))
      (if (/= numberLast  "")(setq numberLast  (strcat "numberLast=\""numberLast   "\" ")))
      (if (/= numberSuffixLast "")(setq numberSuffixLast (strcat "numberSuffixLast=\""numberSuffixLast  "\" ")))
      (if (/= roadName "")(setq roadName (strcat "roadName=\""roadName  "\" ")))
      (if (/= roadnamesuffix "")(setq roadnamesuffix (strcat "roadNameSuffix=\""roadnamesuffix "\" ")))
      (if (/= roadnametype "")(setq roadnametype (strcat "roadNameType=\""roadnametype  "\" ")))
      (if (/= roadtype "")(setq roadtype (strcat "roadType=\""roadtype  "\" ")))
      (if (/= adminareaname "")(setq adminareaname (strcat "adminAreaName=\""adminareaname  "\" ")))
      (if (/= adminAreaCode "")(setq adminAreaCode (strcat "adminAreaCode=\""adminAreaCode  "\" ")))

      (setq largeaddress (strcat "<LocationAddress addressType=\"Primary\" " flatnumber flattype floorlevelnumber floorleveltype numberfirst numbersuffixfirst numberlast numbersuffixlast ">" (chr 10)
				 "<RoadName " roadname roadnamesuffix roadnametype roadtype "/>" (chr 10)
				 "<AdministrativeArea " adminareaname "adminAreaType=\"Locality\" " adminareacode "/>" (chr 10)
				 "</LocationAddress>"))
      )
      (setq largeaddress "")
		)


			 
			 
			 (setq oclist (vl-sort oclist '<))
			 (setq lotlist (append lotlist (list ocmain)))
			 (setq lotlist (append lotlist (list "    <Parcels>")))
			 
			 (setq occount 0)
			 (repeat (length oclist)
			   (setq oc (nth occount oclist))
			   (setq lotlist (append lotlist (list (strcat "      <Parcel name=\"LNK-" (rtos linkno 2 0) "\"" oc  ))))
                           (setq linkno (+ linkno 1))

			     (setq occount (+ occount 1))
			        
     

  );r
			 (setq lotlist (append lotlist (list "    </Parcels>" )))
			 (setq lotlist (append lotlist (list largeaddress)))
			(setq lotlist (append lotlist (list "    </Parcel>" )))
			 ));if ocmain exists

  

 




  

  ;GET XML FILE
 (setq outfile (getfiled "Output File" "" "XML" 1))
  (setq outfile (open outfile "w"))
  
  ;WRITE XML HEADER

   
     (setq d (rtos (getvar "CDATE") 2 6))
     (setq date1 (strcat (substr d 3 2)  "-" (substr d 5 2) "-" (substr d 7 2)))
     (setq time1 (strcat (substr d 10 2) ":" (substr d 12 2) ":" (substr d 14 2)))
  (if (= (strlen time1 ) 7) (setq time1 (strcat time1 "0")));fix trailing zeros removed on tens of seconds
  (if (= (substr time1 7 2) "60") (setq time1 (strcat (substr time1 1 6 ) "00")))
  (if (= (substr time1 7 2) "") (setq time1 (strcat (substr time1 1 6 ) "00")))

    (setq datum (nth 21 attlist))
    (if (/= (setq stringpos (vl-string-search "~" datum )) nil)(progn
     
                      (setq datumdesc (strcat " desc=\""(substr datum (+ stringpos 2) 1000) "\" "))
                      (setq datum (substr datum 1 stringpos))
		      )(setq datumdesc "")
      )
  (princ "\nWriting File Header")
  (write-line "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" outfile)
(write-line (strcat "<LandXML version=\"1.0\" date=\"20" date1 "\" time=\"" time1 "\"" ) outfile)
(write-line  "xmlns=\"http://www.landxml.org/schema/LandXML-1.2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.landxml.org/schema/LandXML-1.2 http://www.landxml.org/schema/LandXML-1.2/LandXML-1.2.xsd\">" outfile)
(write-line "<Units>" outfile)
  (write-line "<Metric linearUnit=\"meter\" temperatureUnit=\"celsius\" volumeUnit=\"cubicMeter\" areaUnit=\"squareMeter\" pressureUnit=\"milliBars\" angularUnit=\"decimal dd.mm.ss\" directionUnit=\"decimal dd.mm.ss\" />" outfile)
(write-line "</Units>" outfile)
(write-line (strcat "<CoordinateSystem" datumdesc " datum=\""datum "\" horizontalDatum=\""( nth 22 attlist)"\" />") outfile)
(write-line "" outfile)
(write-line (strcat "<Application name=\"Landxml for Autocad Victorian Flavour\" version=\"" version "\" />") outfile)
(write-line "<FeatureDictionary name=\"xml-gov-au-vic-icsm-eplan-cif-protocol\" version=\"1.0\" />" outfile)

  
  ;@@@@Write cg points while checking for pms and datum points
    (princ "\nWriting CG points")
  (if (/= (nth 22 attlist) "")(setq zone (strcat "zoneNumber=\"" (nth 23 attlist) "\""))(setq zone ""))
(write-line (strcat "<CgPoints " zone ">") outfile)
(setq count 0)
  (repeat (/ (length cgpl) 4)
    (setq p1s (nth count cgpl))
    (setq pntSurv (nth (+ count 1) cgpl))
    (setq state (nth (+ count 2) cgpl))
    (setq name (nth (+ count 3) cgpl))

    (if (/= (setq datumpoint (cadr (member p1s dpl))) nil)(setq desc (strcat "desc=\""datumpoint "\" "))(setq desc ""))
    (if (and (/= (cadr (member p1s dpl)) nil)(/= (caddr (member p1s dpl)) "NORL"))(setq p1s (strcat p1s  " " (caddr (member p1s dpl)))))
    (if (/= (setq pmnum (cadr (member p1s pmlist))) nil)(setq oid (strcat "oID=\"" pmnum "\" "))
      (setq oid "")
      )
    (setq count (+ count 4))
     
    (write-line (strcat "  <CgPoint state=\""state"\" pntSurv=\"" pntsurv "\" " oid desc "name=\"CGPNT-" name "\">" p1s "</CgPoint>") outfile)
    );r
(write-line "</CgPoints>" outfile)
  (write-line "" outfile)
    (princ "\nWriting Parcels")
  (write-line "<Parcels>" outfile)


  (setq count 0)
  (repeat (length lotlist)
    (write-line (nth count lotlist) outfile)
    (setq count (+ count 1))
    )
    (setq count 0)
  (repeat (length mpolist)
    (write-line (nth count mpolist) outfile)
    (setq count (+ count 1))
    )
  
  
    (write-line "</Parcels>" outfile)

  ;plan features
    (princ "\nWriting Plan Features")
  (if (> (length pflist) 0)(progn
		    (write-line "<PlanFeatures name=\"Feature\">" outfile)
	    (setq count 0)
  (repeat (length pflist)
    (write-line (nth count pflist) outfile)
    (setq count (+ count 1))
    );r
		    (write-line "</PlanFeatures>" outfile)
		    )
    )
   (princ "\nWriting Survey Header")

  (if (= (nth 3 attlist) "")(setq surfirm "")(setq surfirm (strcat " surveyorFirm=\"" (nth 3 attlist) "\" ")))
  (if (= (nth 4 attlist) "")(setq surveyorReference "")(setq surveyorReference (strcat " surveyorReference=\"" (nth 4 attlist) "\" ")))

    (if (= (nth 13 attlist) "partially surveyed")(setq plantype "compiled"))
    (if (= (nth 13 attlist) "compiled")(setq plantype "computed"))
    (if (= (nth 13 attlist) "surveyed")(setq plantype "surveyed"))

  
  ;write survey header  
  ;@@@@surveyorf firm extra "
  (write-line "<Survey>" outfile)
(write-line (strcat "  <SurveyHeader name=\""  (nth 0 attlist) "\" jurisdiction=\"" (nth 19 attlist)   "\" type=\"" plantype "\"" surfirm surveyorReference " surveyFormat=\"" (nth 20 attlist) "\">") outfile)

    ;seperate head of powers by &

    (setq hop (nth 15 attlist))
    ;remove capitals
    (setq hop (vl-string-subst "Subdivision Act 1988" "SUBDIVISION ACT 1988" hop))
    (setq hop (vl-string-subst "Owners Corporation Act 2006" "OWNERS CORPORATION ACT 2006" hop))
    (setq hop (vl-string-subst "Transfer of Land Act 1958" "TRANSFER OF LAND ACT 1958" hop))

	   
    (if (/= nil (vl-string-position 38 hop 0))
      (progn
	(while (/= nil (setq &pos (vl-string-position 38 hop 0)))
	  (progn
	    (setq hops (substr hop 1 (- &pos 1)))
	    (setq hop (substr hop (+ &pos 7) 5000))
	    (if (/= hops "")(write-line (strcat "    <HeadOfPower name=\"" hops "\"/>") outfile))
	    ))))
    (if (/= hop "")(write-line (strcat "    <HeadOfPower name=\"" hop "\"/>") outfile))

 

	  ;uncapitalise section and purpose
	  (setq purpose1 (nth 26 attlist))
	  (setq purpose2 (nth 14 attlist))
   (if (member (strcat purpose1 "-" purpose2) purposelistuncaps)(setq purpose3 (nth 0 (member (strcat purpose1 "-" purpose2) purposelistuncaps)))(setq purpose3(strcat purpose1 "-" purpose2)))

    

   ;REMOVED- NOW DONE BY UNCAPS LIST remove plan name for section 37(8) and (Section 26)	  
;(if (or (= purpose1 ) "Section 37(8)")(= purpose1 "Section 26"))(setq purpose2 "")(setq purpose2 (strcat "-" purpose2))))

	  ;uncapitalise section and purpose
	   
    (if (/= purpose3 "")(write-line (strcat "    <PurposeOfSurvey name=\"" purpose3  "\"/>") outfile))

    
  (SETQ DOS (nth 18 attlist))
  (if (/= dos "")(progn

;sort date entrys
  ;replace /,\,. with -
  (setq /pos 0)
	      (while (/=  (setq /pos (vl-string-search "/" dos /pos )) nil) (setq dos (vl-string-subst "-" "/"  dos /pos)
										      /pos (+ /pos 1)))
(setq /pos 0)
	      (while (/=  (setq /pos (vl-string-search "\\" dos /pos )) nil) (setq dos (vl-string-subst "-" "\\"  dos /pos)
										      /pos (+ /pos 2)))
  (setq /pos 0)
	      (while (/=  (setq /pos (vl-string-search "." dos /pos )) nil) (setq dos (vl-string-subst "-" "."  dos /pos)
										      /pos (+ /pos 1)))
    (setq minuspos1 (vl-string-position 45 dos 0))
  (setq minuspos2 (vl-string-position 45 dos (+ minuspos1 1)))
  (if (or (= minuspos1 1)(= minuspos1 2))(progn;rearrage date
				       (setq day  (substr dos 1 minuspos1))
				       (if (= (strlen day) 1) (setq day (strcat "0" day)));single digit days
				       (setq month (substr dos (+ minuspos1 2) (- (- minuspos2 minuspos1) 1)))
				       (if (= (strlen month) 1) (setq month (strcat "0" month)));single digit days
				       (setq year  (substr dos (+ minuspos2 2) 50))
				       (setq dos (strcat year "-" month "-" day))
				       ));p&if dos round the wrong way
  ));p&if dos not ""
 

 (if (/= DOS "")(write-line (strcat "    <AdministrativeDate adminDateType=\"Date of Survey\" adminDate=\"" DOS "\" />") outfile))

 (if (/= (nth 2 attlist) "")(write-line (strcat "    <AdministrativeArea adminAreaType=\"Parish\" adminAreaName=\"" (nth 2 attlist) "\" adminAreaCode=\"" (nth 25 attlist) "\" />") outfile))
 (if (/= (nth 1 attlist) "")(write-line (strcat "    <AdministrativeArea adminAreaType=\"LGA\" adminAreaName=\"" (nth 1 attlist) "\" adminAreaCode=\"" (nth 24 attlist) "\" />") outfile))
   
  


    (if (/= (nth 16 attlist) "")(write-line (strcat "    <Personnel name=\"" (nth 16 attlist) "\" regType=\"Licensed Cadastral Surveyor\" role=\"Surveyed By\" regNumber=\"" (nth 17 attlist ) "\" />") outfile))

    
     (if (/= (nth 5 attlist) "")(progn
				  (write-line (strcat "    <Annotation type=\"Township\" name=\"ANNO-" (rtos annocount 2 0) "\" desc=\"" (nth 5 attlist) "\"/>") outfile)
				  (setq annocount (+ annocount 1))))
    (if (/= (nth 6 attlist) "")(progn
				  (write-line (strcat "    <Annotation type=\"Crown Section\" name=\"ANNO-" (rtos annocount 2 0) "\" desc=\"" (nth 6 attlist) "\"/>") outfile)
				  (setq annocount (+ annocount 1))))
     (if (/= (nth 7 attlist) "")(progn
				  (write-line (strcat "    <Annotation type=\"Crown Allotment\" name=\"ANNO-" (rtos annocount 2 0) "\" desc=\"" (nth 7 attlist) "\"/>") outfile)
				  (setq annocount (+ annocount 1))))
    (if (/= (nth 8 attlist) "")(progn
				  (write-line (strcat "    <Annotation type=\"Crown Portion\" name=\"ANNO-" (rtos annocount 2 0) "\" desc=\"" (nth 8 attlist) "\"/>") outfile)
				  (setq annocount (+ annocount 1))))



;write all other annotations
  (write-line  annolist outfile)
      

    
  (write-line "  </SurveyHeader>" outfile)

;Write instrument stations to file
    (princ "\nWriting Instrument Stations")
(setq count 0)
  (repeat (/ (length cgpl) 4)
    (setq name (nth (+ count 3) cgpl))

    
    (if (member name islist) (progn
(write-line (strcat "  <InstrumentSetup id=\"IS-" name "\" stationName=\"IS-" name "\" instrumentHeight=\"0\">") outfile)
        (write-line (strcat "<InstrumentPoint pntRef=\"CGPNT-" name "\"/>") outfile)
   (write-line "</InstrumentSetup>" outfile)
));p&if
    (setq count (+ count 4))
    );r
(princ "\nWriting Observations")
  ;write reference observations    
(write-line "<ObservationGroup id=\"OG-1\">" outfile)

  (setq count 0)
  (repeat (length rolist)
    (write-line (strcat "  "(nth count rolist)) outfile)
    (setq count (+ count 1))
    );r
  (write-line "</ObservationGroup>" outfile)
  (write-line "</Survey>" outfile)
    (princ "\nWriting Monuments")
    (IF ( /= monlist nil)(progn
  (write-line "<Monuments>" outfile)

  (setq count 1)
    (repeat (/(length monlist)2)
(write-line (strcat "  "(nth count monlist)) outfile)
      (setq count (+ count 2))
      );r

  (write-line "</Monuments>" outfile)
  ))
(write-line "</LandXML>" outfile)
    (princ "\nClosing File")
(close outfile)

    ;add cgpoints to to drawing

    (princ "\nAdding CG points labels to drawing")
(IF (/= (setq oldcgp (ssget "_X"  '((8 . "CG Points")))) nil)(command "erase" oldcgp ""))

(setvar "clayer" "CG Points")
(setq count 0)
(repeat (/ (length cgpl) 4)
  (setq coord (nth count cgpl))
  
  (setq text (strcat (nth (+ count 3) cgpl)(substr (nth (+ count 1) cgpl) 1 1)(substr (nth (+ count 2) cgpl) 1 1)))
  (setq spcpos (vl-string-position 32 coord 0))
(setq north   (substr coord 1  spcpos  ))
(setq east   (substr coord (+ spcpos 2) 200))
(setq coord (strcat east "," north))
  ;(command "point" coord) removed caused confusion of points
  (COMMAND "TEXT" "J" "BL"  coord (* TH 0.25) "90" text)
(setq count (+ count 4))
  )


    (princ "\nChecking for close vicinity points")
(setq roundlist (list))
    (setq exactlist (list))
    (setq rnd 0.010)
    (setq cvpf "N")
    (setq count 0)
    (repeat (/ (length cgpl) 4)
      (setq pntsurv (nth (+ count 1) cgpl))
      (if (/= pntsurv "sideshot")
	(progn
  (setq coord (nth count cgpl))
    (setq spcpos (vl-string-position 32 coord 0))
(setq north   (atof (substr coord 1  spcpos  )))
(setq east   (atof (substr coord (+ spcpos 2) )))

  ;create rounded values
(setq rnorth (/ north rnd))
    (setq fp (- rnorth (fix rnorth)))
  (if (>= fp 0.5)(setq rnorth (* (+ (fix rnorth) 1) rnd))(setq rnorth (* (fix rnorth) rnd)))
  (setq reast (/ east rnd))
  (setq fp (- reast (fix reast)))
  (if (>= fp 0.5)(setq reast (* (+ (fix reast) 1) rnd))(setq reast (* (fix reast) rnd)))

   (if (and (member (strcat (rtos reast 2 6) "," (rtos rnorth 2 6)) roundlist)(= (member (strcat (rtos east 2 6) "," (rtos north 2 6)) exactlist) nil))
	(progn
    
  (if (= cvpf "N") (progn
			     (setq cvpf "Y")
			     (princ "\nWarning - close points found:")
			     ))
	(princ (strcat " " (rtos east 2 3) "," (rtos  north 2 3)))
	)
	)
  (setq exactlist (append exactlist (list (strcat (rtos east 2 6) "," (rtos north 2 6)))))
  (setq roundlist (append roundlist (list (strcat (rtos reast 2 6) "," (rtos rnorth 2 6)))))
     
  
      ));if sideshot
       (setq count ( + count 4))
      )
    
    
	   (setvar "clayer" prevlayer )
(princ "\nExport Complete")


    
    
    );defun

(setq purposelistuncaps (list
			  "SECTION 6(1)(K)-REMOVAL OF EASEMENT"
"Section 6(1)(K)"
			  "SECTION 22-PLAN OF SUBDIVISION"
"Section 22-Plan of Subdivision"
			  "SECTION 22-PLAN OF CONSOLIDATION"
"Section 22-Plan of Consolidation"
			  "SECTION 23-CREATION OF EASEMENT"
"Section 23-Creation of Easement"
			  "SECTION 23-REMOVAL OF EASEMENT"
"Section 23-Removal of Easement"
			  "SECTION 23-VARIATION OF EASEMENT"
"Section 23-Variation of Easement"
			  "SECTION 23-CREATION AND REMOVAL OF EASEMENT"
"Section 23-Creation and Removal of Easement"
			  "SECTION 23-CREATION AND VARIATION OF EASEMENT"
"Section 23-Creation and Variation of Easement"
			  "SECTION 23-REMOVAL AND VARIATION OF EASEMENT"
"Section 23-Removal and Variation of Easement"
			  "SECTION 23-CREATION AND REMOVAL AND VARIATION OF EASEMENT"
"Section 23-Creation and Removal and Variation of Easement"
			  "SECTION 23-VARIATION OF CONDITION IN CROWN GRANT"
"Section 23-Variation of Condition in Crown Grant"
			  "SECTION 23-REMOVAL OF CONDITION IN CROWN GRANT"
"Section 23-Removal of Condition in Crown Grant"
			  "SECTION 23-CREATION OF RESTRICTION"
"Section 23-Creation of Restriction"
			  "SECTION 23-REMOVAL OF RESTRICTION"
"Section 23-Removal of Restriction"
			  "SECTION 23-VARIATION OF RESTRICTION"
"Section 23-Variation of Restriction"
			  "SECTION 24A-VESTING OF A RESERVE"
"Section 24a-Vesting of a Reserve"
			  "SECTION 24A-REMOVAL OF A RESERVE (PLAN OF SUBDIVISION)"
"Section 24a-Removal of a Reserve (Plan of Subdivision)"
			  "SECTION 24A-REMOVAL OF A RESERVE (PLAN OF CONSOLIDATION)"
"Section 24a-Removal of a Reserve (Plan of Consolidation)"
			  "SECTION 24A-REMOVAL AND VESTING OF A RESERVE (PLAN OF SUBDIVISION)"
"Section 24a-Removal and Vesting of a Reserve (Plan of Subdivision)"
			  "SECTION 24A-REMOVAL AND VESTING OF A RESERVE (PLAN OF CONSOLIDATION)"
"Section 24a-Removal and Vesting of a Reserve (Plan of Consolidation)"
			  "SECTION 26-BOUNDARY PLAN"
"Section 26"
			  "SECTION 32-PLAN TO ALTER LAND AFFECTED BY AN OWNERS CORPORATION (PLAN OF SUBDIVISION)"
"Section 32-Plan to alter land affected by an owners corporation (Plan of Subdivision)"
			  "SECTION 32-PLAN TO ALTER LAND AFFECTED BY AN OWNERS CORPORATION (REGISTERED PLAN)"
"Section 32-Plan to alter land affected by an owners corporation (Registered Plan)"
			  "SECTION 32-PLAN TO ALTER LAND AFFECTED BY AN OWNERS CORPORATION (STRATA PLAN)"
"Section 32-Plan to alter land affected by an owners corporation (Strata Plan)"
			  "SECTION 32-PLAN TO ALTER LAND AFFECTED BY AN OWNERS CORPORATION (CLUSTER SUBDIVISION)"
"Section 32-Plan to alter land affected by an owners corporation (Cluster Subdivision)"
			  "SECTION 32A-PLAN OF SUBDIVISION OF LAND IF AN OWNERS CORPORATION IS AFFECTED"
"Section 32a-Plan of Subdivision of land if an owners corporation is affected"
			  "SECTION 32A-PLAN OF CONSOLIDATION OF LAND IF AN OWNERS CORPORATION IS AFFECTED"
"Section 32a-Plan of Consolidation of land if an owners corporation is affected"
			  "SECTION 32B-PLAN TO CREATE AN OWNERS CORPORATION (EXISTING PLAN)"
"Section 32b-Plan to create an owners corporation (Existing Plan)"
			  "SECTION 32B-PLAN TO CREATE AN OWNERS CORPORATION (NEW PLAN)"
"Section 32b-Plan to create an owners corporation (New Plan)"
			  "SECTION 35-ACQUISITION OF LAND BY ACQUIRING AUTHORITY"
"Section 35-Acquisition of land by acquiring authority"
			  "SECTION 35-ACQUISITION OF LAND IF AN OWNERS CORPORATION IS AFFECTED (PLAN OF SUBDIVISION)"
"Section 35-Acquisition of land if an owners corporation is affected (Plan of Subdivision)"
			  "SECTION 35-ACQUISITION OF LAND IF AN OWNERS CORPORATION IS AFFECTED (REGISTERED PLAN)"
"Section 35-Acquisition of land if an owners corporation is affected (Registered Plan)"
			  "SECTION 35-ACQUISITION OF LAND IF AN OWNERS CORPORATION IS AFFECTED (STRATA PLAN)"
"Section 35-Acquisition of land if an owners corporation is affected (Strata Plan)"
			  "SECTION 35-ACQUISITION OF LAND IF AN OWNERS CORPORATION IS AFFECTED (CLUSTER SUBDIVISION)"
"Section 35-Acquisition of land if an owners corporation is affected (Cluster Subdivision)"
			  "SECTION 35(8)-SUBDIVISION OF LAND VESTED OR REGISTERED IN AUTHORITY"
"Section 35(8)-Subdivision of land vested or registered in authority"
			  "SECTION 35(8)-CONSOLIDATION OF LAND VESTED OR REGISTERED IN AUTHORITY"
"Section 35(8)-Consolidation of land vested or registered in authority"
			  "SECTION 37-PLAN OF SUBDIVISION (STAGED PLAN)"
"Section 37-Plan of Subdivision (Staged Plan)"
			  "SECTION 37-ACQUISITION OF LAND (PLAN OF SUBDIVISION (STAGED))"
"Section 37-Acquisition of land (Plan of Subdivision (Staged))"
			  "SECTION 37(8)-PLAN OF SUBDIVISION (STAGED PLAN)"
"Section 37(8)"
))

(setq purposelist (list
"Section 6(1)(K)"
"Section 22-Plan of Subdivision"
"Section 22-Plan of Consolidation"
"Section 23-Creation of Easement"
"Section 23-Removal of Easement"
"Section 23-Variation of Easement"
"Section 23-Creation and Removal of Easement"
"Section 23-Creation and Variation of Easement"
"Section 23-Removal and Variation of Easement"
"Section 23-Creation and Removal and Variation of Easement"
"Section 23-Variation of Condition in Crown Grant"
"Section 23-Removal of Condition in Crown Grant"
"Section 23-Creation of Restriction"
"Section 23-Removal of Restriction"
"Section 23-Variation of Restriction"
"Section 24a-Vesting of a Reserve"
"Section 24a-Removal of a Reserve (Plan of Subdivision)"
"Section 24a-Removal of a Reserve (Plan of Consolidation)"
"Section 24a-Removal and Vesting of a Reserve (Plan of Subdivision)"
"Section 24a-Removal and Vesting of a Reserve (Plan of Consolidation)"
"Section 26"
"Section 32-Plan to alter land affected by an owners corporation (Plan of Subdivision)"
"Section 32-Plan to alter land affected by an owners corporation (Registered Plan)"
"Section 32-Plan to alter land affected by an owners corporation (Strata Plan)"
"Section 32-Plan to alter land affected by an owners corporation (Cluster Subdivision)"
"Section 32a-Plan of Subdivision of land if an owners corporation is affected"
"Section 32a-Plan of Consolidation of land if an owners corporation is affected"
"Section 32b-Plan to create an owners corporation (Existing Plan)"
"Section 32b-Plan to create an owners corporation (New Plan)"
"Section 35-Acquisition of land by acquiring authority"
"Section 35-Acquisition of land if an owners corporation is affected (Plan of Subdivision)"
"Section 35-Acquisition of land if an owners corporation is affected (Registered Plan)"
"Section 35-Acquisition of land if an owners corporation is affected (Strata Plan)"
"Section 35-Acquisition of land if an owners corporation is affected (Cluster Subdivision)"
"Section 35(8)-Subdivision of land vested or registered in authority"
"Section 35(8)-Consolidation of land vested or registered in authority"
"Section 37-Plan of Subdivision (Staged Plan)"
"Section 37-Acquisition of land (Plan of Subdivision (Staged))"
"Section 37(8)"
))

(setq notelist (list 
"Section 35 - See Recording of Vesting Table Attached"
"Section 35 Compulsory"
"Section 35 Agreement"
"Planning Permit"
"Report on Datum"
"Instrument and Calibration Details"
"Other Crown Description"
"Parcel with Area by Deduction"
"Restriction Expiry Date"
"General Plan Notation"
"Abstract of Field Records Notation"
"Surveyor's Report Notation"
"Title Closure Justification"
"General Easement Notation"
"Easement Qualification"
"Implied Easement Notation"
"Easement Purpose"
"Easement Origin"
"Easement Beneficiary"
"Easement Width"
"Supply of Supplementary Field Record Notation"
"Building Boundary Notation"
"Owners Corporation Notation"
"Additional Purpose of Plan"
"Section 12(2) of the Subdivision Act 1988 applies vide this plan"
"Section 12(2) of the Subdivision Act 1988 does not apply vide this plan"
"Included Stages"
"Purpose Of The Owners Corporation"
"The Basis For Allocation Of Lot Entitlement And Liability"
"Details Of The Limitations Of The Owners Corporation"
"Functions Or Obligations Referred By The Limited Owners Corporation"
"Functions Or Obligations Referred To The Unlimited Owners Corporation"
"Purpose of Plan"
"Grounds for Removal"
"Grounds for Variation"
"Grounds for Vesting"
"Future Plan Number"
))			       

    (setq easepurposelist (list "Air Supply" "Flow of Air" "Passage of Air" "Air Exhaust and Ventilation" "Carriageway" "Drainage" "Drainage and Floodway" "Drainage and Sewerage"
				"Drainage and Waterway" "Erosion" "Fire Access" "Fire Escape" "Fire Egress" "Floodway" "Flooding" "Flow of Light and Air" "Footway"
				"Gas Distribution Pipeline" "Gas Transmission Pipeline" "Supply of Gas" "Flow of Light" "Passage of Light" "Overhanging Eaves"
				"Overhanging Spouting" "Overhanging Balcony" "Party Wall" "Chimney" "Passage of Light and Air" "Pipeline or Ancillary Purposes"
				"Powerline" "Right of Entry" "Sewerage" "Soakage by Water" "Submergence" "Walkway" "Walkway in the event of 'activity' in 'specific location'"
				"Waterway" "Waterway Management" "Way"
				"Group2-Channel" "Group2-Data Transmission" "Group2-Supply of Electricity" "Group2-Transmission of Electricity" "Group2-Ground Water Monitoring"
				"Group2-Irrigation" "Group2-Loading and Unloading Heavy Equipment" "Group2-Mail Collection" "Group2-Overhanging Projections" "Group2-Sanitary Convenience"
				"Group2-Supply of Recycled Water" "Group2-Supply of Water" "Group2-Support" "Group2-Telecommunications" "Group2-Underground Effluent Disposal"
				"Group2-Use of Stairway" "Group2-Vehicle Parking" "Group2-Waste Disposal" "Group2-Wetland" "Group3-Bore, Windmill and Tank" "Group3-Garbage Collection"
				"Group3-Laying Water Pipes" "Group3-Nuisance or Annoyance" "Group3-Public Conveniences" "Group3-Public Highway" "Group3-Quarrying and Blasting"
				"Group3-Recreation and Garden" "Group3-Signboard, Signage" "Group3-Tree Planting" "Group4-Air Exhaust, Ventilation and Access" "Group4-Vehicle Parking"
				"Group4-Carriageway (with Limitation and Obligation)" "Group4-Right of Access"))
       
  (setq laylist (list "2.Generic Plan portrait" "3.Generic Plan-landscape" "4.Abstract of Field Records-front" "5.Abstract of Field Records-subsequent" "6.Other Plan-front"))


(setq formatlist (list "Community Schemes" "Examination Survey" "Standard" "Stratum" "Strata Schemes" "Survey Information Only"))
(setq datumlist (list "Approx True North" "MGA94_Zone54" "MGA94_Zone55" "MGA2020_Zone54" "MGA2020_Zone55" ))
(setq hdatumlist (list "Local" "MGA94_Zone54" "MGA94_Zone55" "MGA2020_Zone54" "MGA2020_Zone55" ))
(setq notetypelist (list "Planning Permit" "Report on Datum" "Instrument and Calibration Details" "Other Crown Description" "Section 12(2) of the Subdivision Act 1988 applies vide this plan" "Section 12(2) of the Subdivision Act 1988 does not apply vide this plan" "Included Stages" "Purpose of Plan" "Grounds for Removal" "Grounds for Variation" "Grounds for Vesting" "Future Plan Number"))
(setq noteandpcllist (list "Easement Qualification" "Easement Purpose" "Easement Beneficiary" "Easement Origin" "Building Boundary Notation" "Restriction Expiry Date"
			   "Purpose Of The Owners Corporation" "The Basis For Allocation Of Lot Entitlement And Liability"
			   "Details Of The Limitations Of The Owners Corporation" "Functions Or Obligations Referred By The Limited Owners Corporation"
			   "Functions Or Obligations Referred To The Unlimited Owners Corporation" "Section 35 Compulsory" "Section 35 Agreement" "Parcel with Area by Deduction"))
(setq nandoppcllist (list "General Plan Notation" "Abstract of Field Records Notation" "Title Closure Justification"
			  "Supply of Supplementary Field Record Notation" "General Easement Notation" "Section 35 - See Recording of Vesting Table Attached" "Implied Easement Notation"))
(setq titlelist (list "Alpine Lease" "Crown Grant" "Crown Lease" "Depth Limitation" "Easement Benefit" "Freehold" "Freehold(Part)"
		      "Identified" "Mineral 'Excludes'" "Mineral 'Includes'" "Restriction Benefit" "Restriction Burden" "Treasurer's Receipt"))

    
    


(setq parishlist (list
"ACHERON,2001" "ACHERON,2001" "ADDINGTON,2002" "ADJIE,2003" "ADZAR,2004" "AIRE,2005" "ALBACUTYA,2006" "ALBERTON EAST,2007" "ALBERTON WEST,2008" "ALEXANDRA,2009" "ALLAMBEE,2010" "ALLAMBEE EAST,2011" "AMHERST,2012" "AMPHITHEATRE,2013" "ANAKIE,2014" "ANGAHOOK,2015" "ANGORA,2016" "ANNUELLO,2017" "ANNYA,2018" "ARAPILES,2019" "ARARAT,2020"
"ARBUCKLE,2021" "ARBUCKLE,2021" "ARCADIA,2022" "ARCHDALE,2023" "ARDNO,2024" "ARDONACHIE,2025" "AREEGRA,2026" "ARGYLE,2027" "ASCOT,2028" "ASHENS,2029" "AUDLEY,2030" "AVENEL,2031" "AVOCA,2032" "AWONGA,2033" "AXEDALE,2034" "BAANGAL,2035" "BAAWANG,2036" "BABATCHIO,2037" "BAEL BAEL,2038" "BAGSHOT,2039" "BAHGALLAH,2040"
"BAILIESTON,2041" "BAILIESTON,2041" "BAIRNSDALE,2042" "BALLAN,2043" "BALLANGEICH,2044" "BALLAPUR,2045" "BALLARAT,2046" "BALLARK,2047" "BALLENDELLA,2048" "BALLIANG,2049" "BALLOONG,2050" "BALLYROGAN,2051" "BALMATTUM,2052" "BALMORAL,2053" "BALNARRING,2054" "BALROOK,2055" "BALROOTAN,2056" "BAMAWM,2057" "BAMBADIN,2058" "BAMBRA,2059" "BAMGANIE,2060"
"BANANGAL,2061" "BANANGAL,2061" "BANGERANG,2062" "BANU BONYIT,2063" "BANYARMBITE,2064" "BANYENA,2065" "BANYENONG,2066" "BARAMBOGIE,2067" "BARANDUDA,2068" "BARCHAN,2069" "BARGA,2070" "BARING,2071" "BARING NORTH,2072" "BARINGHUP,2073" "BARKLY,2074" "BARMAH,2075" "BARNAWARTHA NORTH,2076" "BARNAWARTHA SOUTH,2077" "BARNOOLUT,2078" "BARONGAROOK,2079" "BARP,2080"
"BARRAKEE,2081" "BARRAKEE,2081" "BARRAMUNGA,2082" "BARRARBOOL,2083" "BARROWORN,2084" "BARWIDGEE,2085" "BARWITE,2086" "BARWO,2087" "BARWON DOWNS,2088" "BARWONGEMOONG,2089" "BATCHICA,2090" "BATYIK,2091" "BAULKAMAUGH,2092" "BAW BAW,2093" "BAYNTON,2094" "BEALIBA,2095" "BEAUFORT,2096" "BECKWORTH,2097" "BEEAR,2098" "BEECHWORTH,2099" "BEEK BEEK,2100"
"BEENAK,2101" "BEENAK,2101" "BEERIK,2102" "BEETHANG,2103" "BEEWAR,2104" "BELFAST,2105" "BELLARINE,2106" "BELLAURA,2107" "BELLELLEN,2108" "BELOKA,2109" "BELVOIR WEST,2110" "BEMBOKA,2111" "BEMM,2112" "BENALLA,2113" "BENAMBRA,2114" "BENAYEO,2115" "BENDOCK,2116" "BENETOOK,2117" "BENGWORDEN,2118" "BENGWORDEN SOUTH,2119" "BENJEROOP,2120"
"BEOLITE,2121" "BEOLITE,2121" "BEPCHA,2122" "BEREMBOKE,2123" "BERONTHA,2124" "BERRIMAL,2125" "BERRINGA,2126" "BERRINGAMA,2127" "BERRIWILLOCK,2128" "BERRMARR,2129" "BERROOK,2130" "BERWICK,2131" "BESSIEBELLE,2132" "BET BET,2133" "BETE BOLONG NORTH,2134" "BETE BOLONG SOUTH,2135" "BETKA,2136" "BEULAH,2137" "BEYAL,2138" "BIDWELL,2139" "BIG BILLY,2140"
"BIG DESERT,2141" "BIG DESERT,2141" "BIL-BIL-WYT,2142" "BILLABONG,2143" "BILLIAN,2144" "BILLIMINAH,2145" "BILPAH,2146" "BIMBOURIE,2147" "BINDI,2148" "BINGINWARRI,2149" "BINGO-MUNJIE,2150" "BINGO-MUNJIE NORTH,2151" "BINGO-MUNJIE SOUTH,2152" "BINNICAN,2153" "BINNUC,2154" "BIRREGUN,2155" "BIRREGURRA,2156" "BITCHIGAL,2157" "BITTERANG,2158" "BITTERN,2159" "BLACKWOOD,2160"
"BOCHARA,2161" "BOCHARA,2161" "BOGA,2162" "BOGALARA,2163" "BOGONG NORTH,2164" "BOGONG SOUTH,2165" "BOHO,2166" "BOIGBEAT,2167" "BOIKERBERT,2168" "BOINKA,2169" "BOLAIRA,2170" "BOLANGUM,2171" "BOLERCH,2172" "BOLGA,2173" "BOLLINDA,2174" "BOLWARRA,2175" "BONANG,2176" "BONDI,2177" "BONEGILLA,2178" "BONN,2179" "BONTHERAMBO,2180"
"BOODYARN,2181" "BOODYARN,2181" "BOOLA BOLOKE,2182" "BOOLA BOOLA,2183" "BOOLE POOLE,2184" "BOOLUNGAL,2185" "BOOMAHNOOMOONAH,2186" "BOONAH,2187" "BOONAHWAH,2188" "BOONDEROOT,2189" "BOORAN,2190" "BOORGUNYAH,2191" "BOORHAMAN,2192" "BOORLEE,2193" "BOOROLITE,2194" "BOORONG,2195" "BOORONGIE,2196" "BOOROOPKI,2197" "BOORPOOL,2198" "BOORPUK,2199" "BOORT,2200"
"BOOSEY,2201" "BOOSEY,2201" "BOOTAHPOOL,2202" "BORAMBORAM,2203" "BOREANG EAST,2204" "BOREANG WEST,2205" "BORHONEYGHURK,2206" "BORODOMANIN,2207" "BOROKA,2208" "BOROONDARA,2209" "BORRIYALLOAK,2210" "BORUNG,2211" "BOULKA,2212" "BOURKA,2213" "BOWEYA,2214" "BOW-WORRUNG,2215" "BRADFORD,2216" "BRALAK,2217" "BRAMBURRA,2218" "BRAMBY,2219" "BRANJEE,2220"
"BRANKEET,2221" "BRANKEET,2221" "BRANXHOLME,2222" "BRENANAH,2223" "BREWSTER,2224" "BRIAGOLONG,2225" "BRIDGEWATER,2226" "BRIGHT,2227" "BRIM BRIM,2230" "BRIMBOAL,2228" "BRIMBONGA,2229" "BRIMIN,2231" "BRINDAT,2232" "BRINGALBART,2233" "BRIT BRIT,2234" "BROADFORD,2235" "BROADLANDS,2236" "BROADWATER,2237" "BROCKIE,2238" "BRUARONG,2239" "BRUCKNELL,2240"
"BRUK BRUK,2241" "BRUK BRUK,2241" "BRUTHEN,2242" "BUANGOR,2243" "BUCHAN,2244" "BUCKENDERRA,2245" "BUCKERAN YARRACK,2246" "BUCKLAND,2247" "BUCKRABANYULE,2248" "BUDGEE BUDGEE,2249" "BUDGEREE,2250" "BUDGERUM EAST,2251" "BUDGERUM WEST,2252" "BULART,2253" "BULBAN,2254" "BULGA,2255" "BULGABACK,2256" "BULGANA,2257" "BULLA BULLA,2258" "BULLAMALK,2259" "BULLANBUL,2260"
"BULLAROOK,2261" "BULLAROOK,2261" "BULLARTO,2262" "BULLAWIN,2263" "BULLEEN,2264" "BULLENGAROOK,2265" "BULLIOH,2266" "BULLUMWAAL,2267" "BULLUNG,2268" "BUMBANG,2269" "BUMBERRAH,2270" "BUNDALAGUAH,2271" "BUNDALONG,2272" "BUNDARA-MUNJIE,2273" "BUNDOWRA,2274" "BUNG BONG,2280" "BUNGAL,2275" "BUNGALALLY,2276" "BUNGAMERO,2277" "BUNGANAIL,2278" "BUNGAREE,2279"
"BUNGEELTAP,2281" "BUNGEELTAP,2281" "BUNGEET,2282" "BUNGIL,2283" "BUNGIL EAST,2284" "BUNGULUKE,2285" "BUNGYWARR,2286" "BUNINYONG,2287" "BUNNUGAL,2288" "BUNUROUK,2289" "BUNYIP,2290" "BURAGWONDUC,2291" "BURGOYNE,2292" "BURKE,2293" "BURNELL,2294" "BURNEWANG,2295" "BURRA,2296" "BURRAH BURRAH,2297" "BURRAMBOOT,2298" "BURRAMBOOT EAST,2299" "BURRAMINE,2300"
"BURRONG NORTH,2301" "BURRONG NORTH,2301" "BURRONG SOUTH,2302" "BURROWYE,2303" "BURRUM BURRUM,2306" "BURRUMBEEP,2304" "BURRUMBEET,2305" "BURRUNGABUGGE,2307" "BURTWARRAH,2308" "BURUPGA,2309" "BUTGULLA,2310" "BUTTLEJORRK,2311" "BUXTON,2312" "BYADUK,2313" "BYAMBYNEE,2314" "BYANGA,2315" "BYAWATHA,2316" "BYJUKE,2317" "BYLANDS,2318" "CABANANDRA,2319" "CALIVIL,2320"
"CALLAWADDA,2321" "CALLAWADDA,2321" "CALLIGNEE,2322" "CAMBACANYA,2323" "CAMBATONG,2324" "CAMPASPE,2325" "CAMPBELLTOWN,2326" "CANABORE,2327" "CANIAMBO,2328" "CANNIE,2329" "CANNUM,2330" "CANTALA,2331" "CARAG CARAG,2332" "CARALULUP,2333" "CARAMBALLUC NORTH,2334" "CARAMBALLUC SOUTH,2335" "CARAMUT,2336" "CARAMUT SOUTH,2337" "CARAPOOEE,2338" "CARAPOOEE WEST,2339" "CARAPOOK,2340"
"CARAPUGNA,2341" "CARAPUGNA,2341" "CARBOOR,2342" "CARCHAP,2343" "CARDIGAN,2344" "CARGERIE,2345" "CARINA,2346" "CARISBROOK,2347" "CARLSRUHE,2348" "CARLYLE,2349" "CARNEEK,2350" "CARNGHAM,2351" "CAROOL,2352" "CARORI,2353" "CARPENDEIT,2354" "CARRAH,2355" "CARRAJUNG,2356" "CARRAK,2357" "CARRARAGARMUNGEE,2358" "CARRON,2359" "CARRUNG-E-MURNONG,2360"
"CARRUNO,2361" "CARRUNO,2361" "CARWARP,2362" "CARWARP WEST,2363" "CASTERTON,2364" "CASTLE DONNINGTON,2365" "CASTLEMAINE,2366" "CATIABRIM,2367" "CAVENDISH,2368" "CHANGUE,2369" "CHANGUE EAST,2370" "CHARAM,2371" "CHARLTON EAST,2372" "CHARLTON WEST,2373" "CHATSWORTH,2374" "CHATSWORTH WEST,2375" "CHEPSTOWE,2376" "CHERRINGTON,2377" "CHEWTON,2378" "CHILLINGOLLAH,2379" "CHILPIN,2380"
"CHILTERN,2381" "CHILTERN,2381" "CHILTERN WEST,2382" "CHINAMAN FLAT,2383" "CHINANGIN,2384" "CHINTIN,2385" "CHIPRICK,2386" "CLARENDON,2387" "CLARKESDALE,2388" "CLONBINANE,2389" "CLONLEIGH,2390" "CLUNES,2391" "COBAW,2392" "COBBANNAH,2393" "COBBOBOONEE,2394" "COBON,2395" "COBRA KILLUC,2396" "COBRAM,2397" "COBUNGRA,2398" "COCAMBA,2399" "COCOMAH,2400"
"COCOROC,2401" "COCOROC,2401" "CODRINGTON,2402" "COHUNA,2403" "COIMADAI,2404" "COLAC,2405" "COLAC COLAC,2406" "COLBINABBIN,2407" "COLERAINE,2408" "COLIBAN,2409" "COLIGNAN,2410" "COLONGULAC,2411" "COLQUHOUN,2412" "COLQUHOUN EAST,2413" "COLQUHOUN NORTH,2414" "COLVINSBY,2415" "COMBIENBAR,2416" "COMMERALGHIP,2417" "CONCONGELLA,2418" "CONCONGELLA SOUTH,2419" "CONDAH,2420"
"CONEWARRE,2421" "CONEWARRE,2421" "CONGA WONGA,2422" "CONGUPNA,2423" "CONNANGORACH,2424" "CONNEWARREN,2425" "CONNEWIRRECOO,2426" "COOACK,2427" "COOAGGALAH,2428" "COOLEBARGHURK,2429" "COOLUMBOOKA,2430" "COOLUNGOOLUN,2431" "COOLUNGUBRA,2432" "COOMA,2433" "COOMBOONA,2434" "COONGULLA,2435" "COONGULMERANG,2436" "COONIMUR,2437" "COONOOER EAST,2438" "COONOOER WEST,2439" "COOPRACAMBRA,2440"
"COORAMOOK,2441" "COORAMOOK,2441" "COORIEJONG,2442" "COORNBURT,2443" "COORNMILL,2444" "COOROOPAJERRUP,2445" "COPI PLAINS,2446" "CORACK,2447" "CORACK EAST,2448" "CORADJIL,2449" "CORANGAMITE,2450" "COREA,2451" "CORINDHAP,2452" "CORINELLA,2453" "CORIO,2454" "CORNELLA,2455" "COROP,2456" "CORRYONG,2457" "COSTERFIELD,2458" "COWA,2459" "COYNALLAN,2460"
"CRAIGIE,2461" "CRAIGIE,2461" "CRANBOURNE,2462" "CRESSY,2463" "CRESWICK,2464" "CRONOMBY,2465" "CROOKAYAN,2466" "CROSBIE,2467" "CROWLANDS,2468" "CROXTON EAST,2469" "CROXTON WEST,2470" "CUDGEWA,2471" "CUNDARE,2472" "CURLIP,2473" "CURRACURT,2474" "CURRAWA,2475" "CURTAYNE,2476" "CURYO,2477" "CUT-PAW-PAW,2478" "DAAHL,2479" "DAALKO,2480"
"DAHWEDARRE,2481" "DAHWEDARRE,2481" "DALYENONG,2482" "DANDENONG,2483" "DANDONGADALE,2484" "DANYO,2485" "DARBALANG,2486" "DARGALONG,2487" "DARGILE,2488" "DARGO,2489" "DARKBONEE,2490" "DARLINGFORD,2491" "DARLINGTON,2492" "DARLINGTON WEST,2493" "DARNUM,2494" "DARRAGAN,2495" "DARRAWEIT GUIM,2496" "DARRIMAN,2497" "DARRIWIL,2498" "DARTAGOOK,2499" "DARTELLA,2500"
"DARTMOOR,2501" "DARTMOOR,2501" "DATTUCK,2502" "DEAN,2503" "DEDDICK,2504" "DEDERANG,2505" "DELATITE,2506" "DELLICKNORA,2507" "DENISON,2508" "DENNYING,2509" "DERBY,2510" "DEREEL,2511" "DERGHOLM,2512" "DERING,2513" "DERNDANG,2514" "DERRIL,2515" "DERRIMUT,2516" "DETARKA,2517" "DEUTGAM,2518" "DEVENISH,2519" "DEVON,2520"
"DEWRANG,2521" "DEWRANG,2521" "DIGBY,2522" "DIGGORRA,2523" "DIMBOOLA,2524" "DING-A-DING,2525" "DINGEE,2526" "DINYARRAK,2527" "DJERRIWARRH,2528" "DOBOOBETIC,2529" "DOLEDROOK,2530" "DOLLIN,2531" "DONALD,2532" "DOODWUK,2533" "DOOEN,2534" "DOOKIE,2535" "DOOLAM,2536" "DOOMBURRIM,2537" "DOPEWORA,2538" "DORCHAP,2539" "DOROQ,2540"
"DOUTTA GALLA,2541" "DOUTTA GALLA,2541" "DOWLING FOREST,2542" "DRAJURK,2543" "DREEITE,2544" "DRIK DRIK,2545" "DROPMORE,2546" "DROUIN EAST,2547" "DROUIN WEST,2548" "DRUMANURE,2549" "DRUMBORG,2550" "DRUMDLEMARA,2551" "DRUMMOND,2552" "DRUNG DRUNG,2553" "DUCHEMBEGARRA,2554" "DUDDO,2555" "DUERAN,2556" "DUERAN EAST,2557" "DULUNGALONG,2558" "DUMBALK,2559" "DUNBULBALANE,2560"
"DUNEED,2561" "DUNEED,2561" "DUNKELD,2562" "DUNMORE,2563" "DUNMUNKLE,2564" "DUNNAWALLA,2565" "DUNNEWORTHY,2566" "DUNOLLY,2567" "DURDIDWARRAH,2568" "DURNDAL,2569" "DURONG,2570" "ECHUCA NORTH,2571" "ECHUCA SOUTH,2572" "ECKLIN,2573" "EDDINGTON,2574" "EDENHOPE,2575" "EDGECOMBE,2576" "EDI,2577" "EGERTON,2578" "EGLINTON,2579" "EILDON,2580"
"EILYAR,2581" "EILYAR,2581" "EL DORADO,2582" "ELINGAMITE,2583" "ELLERSLIE,2584" "ELLESMERE,2585" "ELLIMINYT,2586" "ELLINGING,2587" "ELMORE,2588" "ELPHINSTONE,2589" "EMBERTON,2590" "ENANO,2591" "ENFIELD,2592" "ENOCHS POINT,2593" "ENSAY,2594" "ENUC,2595" "EPPALOCK,2596" "ERCILDOUN,2597" "ERRINUNDRA,2598" "ESTCOURT,2599" "ETTRICK,2600"
"EUCAMBENE,2601" "EUCAMBENE,2601" "EUMANA,2602" "EUMEMMERRING,2603" "EUMERALLA,2604" "EURAMBEEN,2605" "EURANDELONG,2606" "EUREKA,2607" "EUROA,2608" "EVERSLEY,2609" "EVERTON,2610" "FARADAY,2611" "FINGAL,2612" "FLINDERS,2613" "FLOWERDALE,2614" "FORBES,2615" "FRAMLINGHAM EAST,2616" "FRAMLINGHAM WEST,2617" "FRANKLIN,2618" "FRANKSTON,2619" "FREEBURGH,2620"
"FRENCH ISLAND,2621" "FRENCH ISLAND,2621" "FRYERS,2622" "FUMINA,2623" "FUMINA NORTH,2624" "GAALANUNGAH,2625" "GABO,2626" "GALAQUIL,2627" "GALICK,2628" "GALLA,2629" "GALPUNGA,2630" "GAMA,2631" "GAMPOLA,2632" "GANNAWARRA,2633" "GANOO GANOO,2634" "GARRATANBUNELL,2635" "GARVOC,2636" "GATUM GATUM,2637" "GAYFIELD,2638" "GEELENGLA,2639" "GEERA,2640"
"GEERAK,2641" "GEERAK,2641" "GELANTIPY EAST,2642" "GELANTIPY WEST,2643" "GELLIBRAND,2644" "GEMBROOK,2645" "GERAHMIN,2646" "GERANG GERUNG,2648" "GERANGAMETE,2647" "GHERANG GHERANG,2649" "GHERINEGHAP,2650" "GHIN GHIN,2651" "GIBBO,2652" "GIFFARD,2653" "GILLINGALL,2654" "GILLUM,2655" "GIMPA,2656" "GINAP,2657" "GINGIMRICK,2658" "GINQUAM,2659" "GIRGARRE,2660"
"GIRGARRE EAST,2661" "GIRGARRE EAST,2661" "GISBORNE,2662" "GLENALADALE,2663" "GLENALBYN,2664" "GLENAROUA,2665" "GLENAULIN,2666" "GLENBURNIE,2667" "GLENCOE,2668" "GLENCOE SOUTH,2669" "GLENDALE,2670" "GLENDARUEL,2671" "GLENDHU,2672" "GLENELG,2673" "GLENGOWER,2674" "GLENHOPE,2675" "GLENLOGIE,2676" "GLENLOTH,2677" "GLENLYON,2678" "GLENMAGGIE,2679" "GLENMONA,2680"
"GLENMORE,2681" "GLENMORE,2681" "GLENORCHY,2682" "GLENORMISTON,2683" "GLENPATRICK,2684" "GLENROWEN,2685" "GLENWATTS,2686" "GLYNWYLLN,2687" "GNARKEET,2688" "GNARR,2689" "GNARWARRE,2690" "GNARWEE,2691" "GOBARUP,2692" "GOBUR,2693" "GOLDIE,2694" "GOLTON GOLTON,2695" "GONZAGA,2696" "GOOLENGOOK,2697" "GOOMALIBEE,2698" "GOON NURE,2702" "GOONEGUL,2699"
"GOONGEE,2700" "GOONGEE,2700" "GOONGERAH,2701" "GOORAM GOORAM GONG,2705" "GOORAMADDA,2703" "GOORAMBAT,2704" "GOORNONG,2706" "GORAE,2707" "GOROKE,2708" "GORONG,2709" "GORRINN,2710" "GORROCKBURKGHAP,2711" "GORYA,2712" "GOULBURN,2713" "GOWANGARDIE,2714" "GOWAR,2715" "GOYURA,2716" "GRACEDALE,2717" "GRAHAM,2718" "GRANTON,2719" "GRANYA,2720"
"GRASSDALE,2721" "GRASSDALE,2721" "GRE GRE,2725" "GREDGWIN,2722" "GREENHILLS,2723" "GREENSBOROUGH,2724" "GRETA,2726" "GRINGEGALGONA,2727" "GRITJURK,2728" "GRUYERE,2729" "GUILDFORD,2730" "GUNAMALARY,2731" "GUNBOWER,2732" "GUNBOWER WEST,2733" "GUNDOWRING,2734" "GUNGARLAN,2735" "GUNYAH GUNYAH,2736" "GUTCHU,2737" "GUTTAMURRA,2738" "GYMBOWEN,2739" "HADDON,2740"
"HAMILTON NORTH,2741" "HAMILTON NORTH,2741" "HAMILTON SOUTH,2742" "HARCOURT,2743" "HARRIETVILLE,2744" "HARROW,2745" "HAVELOCK,2746" "HAWKESTONE,2747" "HAYANMI,2748" "HAZELWOOD,2749" "HEATHCOTE,2750" "HELENDOITE,2751" "HENSLEY,2752" "HESSE,2753" "HEXHAM EAST,2754" "HEXHAM WEST,2755" "HEYWOOD,2756" "HILGAY,2757" "HINDMARSH,2758" "HINNO-MUNJIE,2759" "HOLCOMBE,2760"
"HOLDEN,2761" "HOLDEN,2761" "HOLEY PLAINS,2762" "HOMERTON,2763" "HORSHAM,2764" "HOTHAM,2765" "HOTSPUR,2766" "HOWITT PLAINS,2767" "HOWQUA,2768" "HOWQUA WEST,2769" "HUNTLY,2770" "ILLAWARRA,2771" "INDI,2772" "INGEEGOODBEE,2773" "INGLEWOOD,2774" "IRREWARRA,2775" "IRREWILLIPE,2776" "JALLAKIN,2777" "JALLUKAR,2778" "JALUR,2779" "JAMIESON,2780"
"JAN JUC,2784" "JAN JUC,2784" "JANCOURT,2781" "JANIEMBER EAST,2782" "JANIEMBER WEST,2783" "JARKLAN,2785" "JEERALANG,2786" "JEETHO,2787" "JEETHO WEST,2788" "JEFFCOTT,2789" "JELLALABAD,2790" "JEMBA,2791" "JENNAWARRA,2792" "JEPARIT,2793" "JERRYWAROOK,2794" "JERUK,2795" "JIKA JIKA,2796" "JIL JIL,2797" "JILPANGER,2798" "JILWAIN,2799" "JINDERBOINE,2800"
"JINDIVICK,2801" "JINDIVICK,2801" "JINGALLALA,2802" "JINJELLIC,2803" "JIRNKEE,2804" "JIRRAH,2805" "JOEL JOEL,2806" "JOOP,2807" "JUMBUK,2808" "JUMBUNNA,2809" "JUMBUNNA EAST,2810" "JUNG JUNG,2811" "JUNGKUM,2812" "KAANGLANG,2813" "KAARIMBA,2814" "KADNOOK,2815" "KAERWUT,2816" "KALADBRO,2817" "KALINGUR,2818" "KALK KALK,2821" "KALKALLO,2819"
"KALKEE,2820" "KALKEE,2820" "KALLERY,2822" "KALPIENUNG,2823" "KALYMNA,2824" "KAMAROOKA,2825" "KANAWALLA,2826" "KANAWINKA,2827" "KANCOBIN,2828" "KANEIRA,2829" "KANGDERAAR,2830" "KANGERONG,2831" "KANGERTONG,2832" "KANIVA,2833" "KANYAPELLA,2834" "KAPONG,2835" "KARABEAL,2836" "KARADOC,2837" "KARAWAH,2838" "KARAWINNA,2839" "KARIAH,2840"
"KARLO,2841" "KARLO,2841" "KARNAK,2842" "KARNGUN,2843" "KARRABUMET,2844" "KARRAMOMUS,2845" "KARUP KARUP,2846" "KARWEEN,2847" "KARYRIE,2848" "KATAMATITE,2849" "KATANDRA,2850" "KATTYOONG,2851" "KATUNGA,2852" "KATYIL,2853" "KAY,2854" "KEELANGIE,2855" "KEELBUNDORA,2856" "KEILAMBETE,2857" "KELFEERA,2858" "KELLALAC,2859" "KENMARE,2860"
"KENTBRUCK,2861" "KENTBRUCK,2861" "KERANG,2862" "KERGUNYAH,2863" "KERGUNYAH NORTH,2864" "KERRIE,2865" "KERRISDALE,2866" "KERRIT BAREET,2867" "KEVINGTON,2868" "KEWELL EAST,2869" "KEWELL WEST,2870" "KIA,2871" "KIALLA,2872" "KIANEEK,2873" "KIATA,2874" "KILLARA,2875" "KILLAWARRA,2876" "KILLINGWORTH,2877" "KILNOORAT,2878" "KIMBOLTON,2879" "KINABULLA,2880"
"KINGLAKE,2881" "KINGLAKE,2881" "KINGOWER,2882" "KINIMAKATKA,2883" "KINKELLA,2884" "KINYPANIAL,2885" "KIORA,2886" "KIRKELLA,2887" "KIRKENONG,2888" "KIRRAK,2889" "KNAAWING,2890" "KNOCKWOOD,2891" "KNOWSLEY,2892" "KNOWSLEY EAST,2893" "KOBYBOYN,2894" "KOETONG,2895" "KOIMBO,2896" "KOLEYA,2897" "KOLORA,2898" "KONARDIN,2899" "KONGBOOL,2900"
"KONGWAK,2901" "KONGWAK,2901" "KONNEPRA,2902" "KONONG WOOTONG,2903" "KOOEM,2904" "KOOLA,2905" "KOOLOMERT,2906" "KOOMBERAR,2907" "KOONDA,2908" "KOONIK KOONIK,2910" "KOONIKA,2909" "KOORAGAN,2911" "KOORANGIE,2912" "KOOREH,2913" "KOORKAB,2914" "KOOROC,2915" "KOOROOL,2916" "KOOROOMAN,2917" "KOOROON,2918" "KOORT-KOORT-NONG,2919" "KOO-WEE-RUP,2920"
"KOO-WEE-RUP EAST,2921" "KOO-WEE-RUP EAST,2921" "KORKUPERRIMUL,2922" "KORNONG,2923" "KORO-GANEIT,2924" "KOROIT,2925" "KORONG,2926" "KOROROIT,2927" "KORRAK KORRAK,2928" "KORUMBURRA,2929" "KORWEINGUBOORA,2930" "KOSCIUSKO,2931" "KOTUPNA,2932" "KOUT NARIN,2933" "KOWAT,2934" "KOYUGA,2935" "KRAMBRUK,2936" "KUARK,2937" "KULK,2938" "KULKYNE,2939" "KULWIN,2940"
"KUNAT KUNAT,2941" "KUNAT KUNAT,2941" "KURDGWEECHEE,2942" "KURNBRUNIN,2943" "KURNWILL,2944" "KURRACA,2945" "KURTING,2946" "KURUC-A-RUC,2947" "KYABRAM,2948" "KYABRAM EAST,2949" "KYBEYAN,2950" "LA TROBE,2977" "LAANECOORIE,2951" "LAANG,2952" "LACEBY,2953" "LAEN,2954" "LAH-ARUM,2955" "LAKE LAKE WOLLARD,2956" "LAL LAL,2959" "LALBERT,2957" "LALKALDARNO,2958"
"LALLAT,2960" "LALLAT,2960" "LAMBRUK,2961" "LANCEFIELD,2962" "LANDSBOROUGH,2963" "LANG LANG,2968" "LANG LANG EAST,2969" "LANGI LOGAN,2966" "LANGI-GHIRAN,2964" "LANGI-KAL-KAL,2965" "LANGKOOP,2967" "LANGLEY,2970" "LANGULAC,2971" "LANGWARRIN,2972" "LANGWORNOR,2973" "LARA,2974" "LARNEEBUNYAH,2975" "LARUNDEL,2976" "LAURAVILLE,2978" "LAURISTON,2979" "LAWALUK,2980"
"LAWLOIT,2981" "LAWLOIT,2981" "LAZARINI,2982" "LEAGHUR,2983" "LEDCOURT,2984" "LEEOR,2985" "LEICHARDT,2986" "LEONGATHA,2987" "LEXINGTON,2988" "LEXTON,2989" "LIANIDUCK,2990" "LICOLA,2991" "LICOLA NORTH,2992" "LIGAR,2993" "LILLICUR,2994" "LILLIMUR,2995" "LILLIPUT,2996" "LILLIRIE,2997" "LIMA,2998" "LINLITHGOW,2999" "LINTON,3000"
"LIPAROO,3001" "LIPAROO,3001" "LISMORE,3002" "LITTLE BILLY,3003" "LIVINGSTONE,3004" "LOCHIEL,3005" "LOCKWOOD,3006" "LODDON,3007" "LODGE PARK,3008" "LONGERENONG,3009" "LONGFORD,3010" "LONGWARRY,3011" "LONGWOOD,3012" "LOOMAT,3013" "LOONGELAAT,3014" "LORNE,3015" "LORQUON,3016" "LOWAN,3017" "LOWRY,3018" "LOY YANG,3020" "LOYOLA,3019"
"LUDRIK-MUNJIE,3021" "LUDRIK-MUNJIE,3021" "LURG,3022" "LYELL,3023" "LYNCHFIELD,3024" "LYNDHURST,3025" "MACARTHUR,3026" "MACEDON,3027" "MACORNA,3028" "MAFFRA,3029" "MAGDALA,3030" "MAGDALA SOUTH,3031" "MAGEPPA,3032" "MAGORRA,3033" "MAHARATTA,3034" "MAHKWALLOK,3035" "MAHRONG,3036" "MAINDAMPLE,3037" "MAINTONGOON,3038" "MAJORLOCK,3039" "MALAKOFF,3040"
"MALANGANEE,3041" "MALANGANEE,3041" "MALDON,3042" "MALKARA,3043" "MALLACOOTA,3044" "MALLANBOOL,3045" "MALLOREN,3046" "MAMBOURIN,3047" "MAMENGOROOCK,3048" "MANANGATANG,3049" "MANANGO,3050" "MANDURANG,3051" "MANEROO,3052" "MANGALORE,3053" "MANNIBADAR,3054" "MANPY,3055" "MANSFIELD,3056" "MANYA,3057" "MARAMINGO,3058" "MARDAN,3059" "MARGOOYA,3060"
"MARIBYRNONG,3061" "MARIBYRNONG,3061" "MARIDA YALLOCK,3062" "MARLBED,3063" "MARLOOH,3064" "MARMA,3065" "MARMAL,3066" "MARNOO,3067" "MARONG,3068" "MARRAWEENY,3069" "MARROO,3070" "MARYBOROUGH,3071" "MARYVALE,3072" "MATLOCK,3073" "MATONG,3074" "MATONG NORTH,3075" "MEATIAN,3076" "MEENIYAN,3077" "MEERAI,3078" "MEEREEK,3079" "MEERING,3080"
"MEERING WEST,3081" "MEERING WEST,3081" "MEERLIEU,3082" "MELBOURNE NORTH,3083" "MELBOURNE SOUTH,3084" "MELLICK-MUNJIE,3085" "MELLIER,3086" "MEPUNGA,3087" "MERAN,3088" "MERBEIN,3089" "MEREDITH,3090" "MERINGUR,3091" "MERINO,3092" "MERRIANG,3093" "MERRIJIG,3094" "MERRIMU,3095" "MERRINEE,3096" "MERRYMBUELA,3097" "MERTON,3098" "METCALFE,3099" "MICKLEHAM,3100"
"MIEPOLL,3101" "MIEPOLL,3101" "MILDURA,3102" "MILLEWA,3103" "MILLOO,3104" "MILMED,3105" "MINAPRE,3106" "MINCHA,3107" "MINCHA WEST,3108" "MINDAI,3109" "MINHAMITE,3110" "MINIMAY,3111" "MININERA,3112" "MINJAH,3113" "MINJAH NORTH,3114" "MINOOK,3115" "MINTO,3116" "MIOWERA,3117" "MIRAMPIRAM,3118" "MIRBOO,3119" "MIRBOO SOUTH,3120"
"MIRIMBAH,3121" "MIRIMBAH,3121" "MIRKOO,3122" "MIRNEE,3123" "MIRRANATWA,3124" "MITCHELL,3125" "MITIAMO,3126" "MITTA MITTA,3127" "MITTYACK,3128" "MITTYAN,3129" "MOAH,3130" "MOALLAACK,3131" "MOCAMBORO,3132" "MOCKINYA,3133" "MODEWARRE,3134" "MOE,3135" "MOGLONEMBY,3136" "MOHICAN,3137" "MOIRA,3138" "MOKANGER,3139" "MOKEPILLY,3140"
"MOKOAN,3141" "MOKOAN,3141" "MOLESWORTH,3142" "MOLIAGUL,3143" "MOLKA,3144" "MOLOGA,3145" "MONBULK,3146" "MONDA,3147" "MONEA NORTH,3148" "MONEA SOUTH,3149" "MONEGEETTA,3150" "MONIVAE,3151" "MONOMAK,3152" "MOOLAP,3153" "MOOLERR,3154" "MOOLORT,3155" "MOOLPAH,3156" "MOOMOWROONG,3157" "MOONDARRA,3158" "MOONIP,3159" "MOONKAN,3160"
"MOONLIGHT,3161" "MOONLIGHT,3161" "MOORA,3162" "MOORABBIN,3163" "MOORADORANOOK,3164" "MOORALLA,3165" "MOORARBOOL EAST,3166" "MOORARBOOL WEST,3167" "MOORBANOOL,3168" "MOOREE,3169" "MOORMBOOL EAST,3170" "MOORMBOOL WEST,3171" "MOORMURNG,3172" "MOORNAPA,3173" "MOORNGAG,3174" "MOOROODUC,3175" "MOOROOLBARK,3176" "MOOROOPNA,3177" "MOOROOPNA WEST,3178" "MOORPANYAL,3179" "MOORTWORRA,3180"
"MOORWINSTOWE,3181" "MOORWINSTOWE,3181" "MORANDING,3182" "MORANG,3183" "MORANGHURK,3184" "MORAY,3185" "MORDIALLOC,3186" "MOREA,3187" "MOREEP,3188" "MOREKANA,3189" "MORKALLA,3190" "MOROCKDONG,3191" "MOROKA,3192" "MORRL MORRL,3193" "MORTAT,3194" "MORTCHUP,3195" "MORTLAKE,3196" "MOSTYN,3197" "MOUNT COLE,3198" "MOURNPOUL,3199" "MOUTAJUP,3200"
"MOUYONG,3201" "MOUYONG,3201" "MOUZIE,3202" "MOWAMBA,3203" "MOYANGUL,3204" "MOYHU,3205" "MOYREISK,3206" "MOYSTON,3207" "MOYSTON WEST,3208" "MUCKLEFORD,3209" "MUDGEEGONGA,3210" "MULCRA,3211" "MULGRAVE,3212" "MULLAGONG,3213" "MULLAWYE,3214" "MULLINDOLINGONG,3215" "MULLROO,3216" "MULLUNGDUNG,3217" "MUMBANNAR,3218" "MUMBEL,3219" "MUNDOONA,3220"
"MUNTHAM,3221" "MUNTHAM,3221" "MURCHISON,3222" "MURCHISON NORTH,3223" "MURDEDUKE,3224" "MURGHEBOLUC,3225" "MURLONG,3226" "MURMUNGEE,3227" "MURNDAL,3228" "MURNUNGIN,3229" "MURRABIT,3230" "MURRABIT WEST,3231" "MURRAMURRANGBONG,3232" "MURRANDARRA,3233" "MURRAWONG,3234" "MURRINDAL EAST,3235" "MURRINDAL WEST,3236" "MURRINDINDI,3237" "MURRNROONG,3238" "MURROON,3239" "MURRUNGOWAR,3240"
"MURTCAIM,3241" "MURTCAIM,3241" "MURYRTYM,3242" "MUSKERRY,3243" "MYALL,3244" "MYAMYN,3245" "MYARING,3246" "MYRNIONG,3247" "MYRRHEE,3248" "MYRTLEFORD,3249" "MYSIA,3250" "NAGWARRY,3251" "NALANGIL,3252" "NANAPUNDAH,3253" "NANDEMARRIMAN,3254" "NANGANA,3255" "NANGEELA,3256" "NANIMIA,3257" "NANNEELLA,3258" "NANOWIE,3259" "NAPIER,3260"
"NAP-NAP-MARRA,3261" "NAP-NAP-MARRA,3261" "NAPPA,3262" "NAR-BE-THONG,3263" "NARBOURAC,3264" "NAREEB NAREEB,3265" "NARIEL,3266" "NARINGANINGALOOK,3267" "NARINGHIL NORTH,3268" "NARINGHIL SOUTH,3269" "NARIOKA,3270" "NARMBOOL,3271" "NAR-NAR-GOON,3272" "NARRACAN,3273" "NARRACAN SOUTH,3274" "NARRANG,3275" "NARRAPORT,3276" "NARRAWATURK,3277" "NARRAWONG,3278" "NARREE WORRAN,3279" "NARREWILLOCK,3280"
"NARROBUK,3281" "NARROBUK,3281" "NARROBUK NORTH,3282" "NARRUNG,3283" "NATEYIP,3284" "NATIMUK,3285" "NATTE MURRANG,3286" "NATTEYALLOCK,3287" "NAVARRE,3288" "NAYOOK,3289" "NAYOOK WEST,3290" "NEEREMAN,3291" "NEERIM,3292" "NEERIM EAST,3293" "NEILBOROUGH,3294" "NEKEEYA,3295" "NENANDIE,3296" "NEPEAN,3297" "NERICK,3298" "NERRAN,3299" "NERRENA,3300"
"NERRIN NERRIN,3302" "NERRIN NERRIN,3302" "NERRING,3301" "NEUARPUR,3303" "NEWHAM,3304" "NEWLINGROOK,3305" "NEWMERELLA,3306" "NGALLO,3307" "NI NI,3312" "NIAGAROON,3308" "NILLAHCOOTIE,3309" "NILLUMBIK,3310" "NINDOO,3311" "NINNIE,3313" "NINYEUNOOK,3314" "NIRRANDA,3315" "NOLAN,3316" "NOOJEE,3317" "NOOJEE EAST,3318" "NOONGA,3319" "NOORILIM,3320"
"NOORINBEE,3321" "NOORINBEE,3321" "NOORONGONG,3322" "NORONG,3323" "NORTHWOOD,3324" "NOWA NOWA,3325" "NOWA NOWA SOUTH,3326" "NOWIE,3327" "NOWINGI,3328" "NOWYEO,3329" "NOYONG,3330" "NULKWYNE,3331" "NULLAN,3332" "NULLAWARRE,3333" "NULLAWIL,3334" "NUMBIE-MUNJIE,3335" "NUMBRUK,3336" "NUNAWADING,3337" "NUNGAL,3338" "NUNGATTA,3339" "NUNNIONG,3340"
"NUNTIN,3341" "NUNTIN,3341" "NURCOUNG,3342" "NURNURNEMAL,3343" "NURONG,3344" "NURRABIEL,3345" "NYALLO,3346" "NYANG,3347" "NYPO,3348" "NYRRABY,3349" "OLANGOLAH,3350" "OLNEY,3351" "OMEO,3352" "ONDIT,3353" "ONYIM,3354" "ORBOST,3355" "ORBOST EAST,3356" "OTWAY,3357" "OUYEN,3358" "OXLEY,3359" "PAARATTE,3360"
"PAIGNIE,3361" "PAIGNIE,3361" "PAINSWICK,3362" "PAKENHAM,3363" "PALLARANG,3364" "PALPARA,3365" "PANBULLA,3366" "PANMURE,3367" "PANNOOBAMAWM,3368" "PANNOOMILLOO,3369" "PANYULE,3370" "PANYYABYR,3371" "PARAPARAP,3372" "PARRIE YALLOAK,3373" "PARUPA,3374" "PARWAN,3375" "PATCHEWOLLOCK,3376" "PATCHEWOLLOCK NORTH,3377" "PATHO,3378" "PAWBYMBYR,3379" "PAYWIT,3380"
"PEECHELBA,3381" "PEECHELBA,3381" "PEECHEMBER,3382" "PELLUEBLA,3383" "PENDYK PENDYK,3384" "PENTAL ISLAND,3386" "PERENNA,3387" "PERRIT PERRIT,3388" "PHILLIP ISLAND,3389" "PIAMBIE,3390" "PIANGIL,3391" "PIANGIL WEST,3392" "PICOLA,3393" "PIER-MILLAN,3394" "PIGICK,3395" "PINE LODGE,3396" "PINES,3397" "PINNAK,3398" "PINNIBAR,3399" "PIRCARRA,3400" "PIRRO,3401"
"PIRRON YALOAK,3402" "PIRRON YALOAK,3402" "POLIAH NORTH,3403" "POLIAH SOUTH,3404" "POLISBET,3405" "POM POM,3408" "POMBORNEIT,3406" "POMPAPIEL,3407" "POMPONDEROO,3409" "POORNEET,3410" "POOWONG,3411" "POOWONG EAST,3412" "POREPUNKAH,3413" "PORTLAND,3414" "POWLETT,3415" "PRAHRAN,3416" "PRANJIP,3417" "PROOINGA,3418" "PROPODOLLAH,3419" "PUCKAPUNYAL,3420" "PUEBLA,3421"
"PULLUT,3422" "PULLUT,3422" "PURDEET,3423" "PURDEET EAST,3424" "PURGAGOOLAH,3425" "PURNIM,3426" "PURNYA,3427" "PURRUMBETE NORTH,3428" "PURRUMBETE SOUTH,3429" "PYALONG,3430" "PYWHEITJORRK,3431" "QUAG-MUNJIE,3432" "QUAMBATOOK,3433" "QUAMBY,3434" "QUAMBY NORTH,3435" "QUANTONG,3436" "QUEENSTOWN,3437" "RAAK,3438" "RAGLAN,3439" "RAGLAN WEST,3440" "RATHSCAR,3441"
"RAVENSWOOD,3442" "RAVENSWOOD,3442" "RED BLUFF,3444" "REDBANK,3443" "REDCASTLE,3445" "REDESDALE,3446" "REDRUTH,3447" "REYNARD,3448" "RIACHELLA,3449" "RICH AVON EAST,3450" "RICH AVON WEST,3451" "RINGWOOD,3452" "ROCHESTER,3453" "ROCHESTER WEST,3454" "ROCHFORD,3455" "RODBOROUGH,3456" "ROSEDALE,3457" "ROSENEATH,3458" "ROTHESAY,3459" "RUFFY,3460" "RUNNYMEDE,3461"
"RUPANYUP,3462" "RUPANYUP,3462" "SALE,3468" "SALISBURY,3469" "SALISBURY WEST,3470" "SAMARIA,3471" "SANDFORD,3472" "SANDHURST,3473" "SANDON,3474" "SARGOOD,3475" "SARSFIELD,3476" "SCARSDALE,3477" "SCORESBY,3478" "SEACOMBE,3479" "SEDGWICK,3480" "SEYMOUR,3481" "SHADFORTH,3482" "SHELBOURNE,3483" "SHELFORD,3484" "SHELFORD WEST,3485" "SHEPPARTON,3486"
"SHERWOOD,3487" "SHERWOOD,3487" "SHIRLEY,3488" "SKIPTON,3489" "SMEATON,3490" "SMYTHESDALE,3491" "SNAKE ISLAND,3492" "SPINIFEX,3493" "SPRING HILL,3495" "SPRING PLAINS,3496" "SPRINGFIELD,3494" "ST. ARNAUD,3463" "ST. CLAIR,3464" "ST. HELENS,3465" "ST. JAMES,3466" "ST. MARGARET,3467" "STANDER,3497" "STANLEY,3498" "STAWELL,3499" "STEAVENSON,3500" "STEWARTON,3501"
"STRADBROKE,3502" "STRADBROKE,3502" "STRANGWAYS,3503" "STRATFORD,3504" "STRATHBOGIE,3505" "STRATHFIELDSAYE,3506" "STRATHMERTON,3507" "STREATHAM,3508" "STRUAN,3509" "SUGGAN BUGGAN,3510" "SUNDAY ISLAND,3511" "SUNSET,3512" "SUTTON,3513" "SUTTON GRANGE,3514" "SWANWATER,3515" "SWITZERLAND,3516" "TAARAAK,3517" "TABBARA,3518" "TABBERABBERA,3519" "TABILK,3520" "TAGGERTY,3521"
"TAHARA,3522" "TAHARA,3522" "TALAMBE,3523" "TALGARNO,3524" "TALGITCHA,3525" "TALLAGEIRA,3526" "TALLANDOON,3527" "TALLANG,3528" "TALLANGALLOOK,3529" "TALLANGATTA,3530" "TALLANGOORK,3531" "TALLAROOK,3532" "TALLYGAROOPNA,3533" "TAMBO,3534" "TAMBOON,3535" "TAMBORITHA,3536" "TAMINICK,3537" "TAMLEUGH,3538" "TANDAROOK,3539" "TANDARRA,3540" "TANGAMBALANGA,3541"
"TANJIL,3542" "TANJIL,3542" "TANJIL EAST,3543" "TAPAROO,3544" "TAPONGA,3545" "TARA,3546" "TARCOMBE,3547" "TARIPTA,3548" "TARKEETH,3549" "TARLDARN,3550" "TARNAGULLA,3551" "TARNEIT,3552" "TARRA TARRA,3557" "TARRAGAL,3553" "TARRANGINNIE,3554" "TARRANGO,3555" "TARRANYURK,3556" "TARRAWARRA,3558" "TARRAWARRA NORTH,3559" "TARRAWINGEE,3560" "TARRAYOUKYAN,3561"
"TARRENGOWER,3562" "TARRENGOWER,3562" "TARWIN,3563" "TARWIN SOUTH,3564" "TATONG,3565" "TATONGA,3566" "TATYOON,3567" "TAWANGA,3568" "TCHIRREE,3569" "TCHUTERR,3570" "TEDDYWADDY,3571" "TELANGATUK,3572" "TELBIT,3573" "TELBIT WEST,3574" "TERANG,3575" "TERLITE-MUNJIE,3576" "TERRAPPEE,3577" "TERRICK TERRICK EAST,3578" "TERRICK TERRICK WEST,3579" "TERRINALLUM,3580" "THALIA,3581"
"THARANBEGGA,3582" "THARANBEGGA,3582" "THEDDORA,3583" "THOLOGOLONG,3584" "THORKIDAAN,3585" "THORNLEY,3586" "THORNTON,3587" "THOWGLA,3588" "THURRA,3589" "TIEGA,3590" "TILDESLEY EAST,3591" "TILDESLEY WEST,3592" "TIMBARRA,3593" "TIMBEROO,3594" "TIMBOON,3595" "TIMMERING,3596" "TINAMBA,3597" "TINGARINGY,3598" "TINTALDRA,3599" "TITTYBONG,3600" "TOLTOL,3601"
"TONG BONG,3604" "TONG BONG,3604" "TONGALA,3602" "TONGARO,3603" "TONGHI,3605" "TONGIO-MUNJIE EAST,3606" "TONGIO-MUNJIE WEST,3607" "TONIMBUK,3608" "TONIMBUK EAST,3609" "TOOAN,3610" "TOOBORAC,3611" "TOOLAMBA,3612" "TOOLAMBA WEST,3613" "TOOLANG,3614" "TOOLIOROOK,3615" "TOOLKA,3616" "TOOLLEEN,3617" "TOOLOME,3618" "TOOLONDO,3619" "TOOLONGROOK,3620" "TOOLOY,3621"
"TOOMBON,3622" "TOOMBON,3622" "TOOMBULLUP,3623" "TOOMBULLUP NORTH,3624" "TOONAMBOOL,3625" "TOONGABBIE NORTH,3626" "TOONGABBIE SOUTH,3627" "TOONGINBOOKA,3628" "TOONYARAK,3629" "TOORA,3630" "TOORAK,3631" "TOORONGO,3632" "TOO-ROUR,3633" "TOOROURRONG,3634" "TOORT,3635" "TORBRECK,3636" "TOTTINGTON,3637" "TOURELLO,3638" "TOWAMBA,3639" "TOWAN,3640" "TOWANINNY,3641"
"TOWANWAY,3642" "TOWANWAY,3642" "TOWMA,3643" "TOWONG,3644" "TRAAWOOL,3645" "TRAGOWEL,3646" "TRARALGON,3647" "TRAWALLA,3648" "TRENTHAM,3649" "TREWALLA,3650" "TRUGANINA,3651" "TUBBUT,3652" "TULILLAH,3653" "TULLAMARINE,3654" "TULLICH,3655" "TULLYVEA,3656" "TUNART,3657" "TUNGIE,3658" "TURANDUREY,3659" "TURKEETH,3660" "TUROAR,3661"
"TURRUMBERRY,3662" "TURRUMBERRY,3662" "TURRUMBERRY NORTH,3663" "TUTEGONG,3664" "TUTYE,3665" "TYABB,3666" "TYAGOOK,3667" "TYALLA,3668" "TYAMOONYA,3669" "TYAR,3670" "TYENNA,3671" "TYIRRA,3672" "TYLDEN,3673" "TYNTYNDER,3674" "TYNTYNDER NORTH,3675" "TYNTYNDER WEST,3676" "TYRENDARRA,3677" "TYRRELL,3678" "ULTIMA,3679" "ULUPNA,3680" "UNDERA,3681"
"UNDERBOOL,3682" "UNDERBOOL,3682" "UNDOWAH,3683" "UPOTIPOTPON,3684" "URANGARA,3685" "VECTIS EAST,3686" "VITE VITE,3687" "WAAIA,3688" "WAANYARRA,3689" "WAARRE,3690" "WABBA,3691" "WABDALLAH,3692" "WABONGA,3693" "WABONGA SOUTH,3694" "WA-DE-LOCK,3695" "WAGANT,3696" "WAGGARANDALL,3697" "WAGRA,3698" "WAHRING,3699" "WAIL,3700" "WAITCHIE,3701"
"WALHALLA,3702" "WALHALLA,3702" "WALHALLA EAST,3703" "WALLA WALLA,3708" "WALLABY,3704" "WALLAGOOT,3705" "WALLALOO,3706" "WALLAN WALLAN,3707" "WALLINDUC,3709" "WALLOWA,3710" "WALLPOLLA,3711" "WALLUP,3712" "WALMER,3713" "WALPA,3714" "WALPAMUNDA,3715" "WALPEUP,3716" "WALWA,3717" "WAMBA,3718" "WANALTA,3719" "WANDILIGONG,3720" "WANDIN YALLOCK,3721"
"WANDO,3722" "WANDO,3722" "WANDOWN,3723" "WANGARABELL,3724" "WANGARATTA NORTH,3725" "WANGARATTA SOUTH,3726" "WANGERRIP,3727" "WANGIE,3728" "WANGOOM,3729" "WANNAEUE,3730" "WANURP,3731" "WANWANDYRA,3732" "WANWIN,3733" "WAPPAN,3734" "WARANGA,3735" "WARATAH,3736" "WARATAH NORTH,3737" "WARBURTON,3738" "WAREEK,3739" "WARGAN,3740" "WARINA,3741"
"WARMUR,3742" "WARMUR,3742" "WARNGAR,3743" "WARRA WARRA,3756" "WARRABKOOK,3744" "WARRACBARUNAH,3745" "WARRACKNABEAL,3746" "WARRAGAMBA,3747" "WARRAGUL,3748" "WARRAIN,3749" "WARRAK,3750" "WARRAMBAT,3751" "WARRAMBINE,3752" "WARRANDYTE,3753" "WARRANOOK,3754" "WARRAQUIL,3755" "WARRAYURE,3757" "WARREEN,3758" "WARRENBAYNE,3759" "WARRENHEIP,3760" "WARRENMANG,3761"
"WARRIMOO,3762" "WARRIMOO,3762" "WARRION,3763" "WARROCK,3764" "WARRONG,3765" "WARROWITUE,3766" "WARTOOK,3767" "WARUNG,3768" "WAT WAT,3778" "WATAEPOOLAN,3769" "WATCHEGATCHECA,3770" "WATCHEM,3771" "WATCHUPGA,3772" "WATEGAT,3773" "WATGANIA,3774" "WATGANIA WEST,3775" "WATHE,3776" "WATTA WELLA,3777" "WAU WAUKA,3779" "WAU WAUKA WEST,3780" "WAYGARA,3781"
"WEDDERBURNE,3782" "WEDDERBURNE,3782" "WEEAPROINAH,3783" "WEECURRA,3784" "WEERAGUA,3785" "WEERANGOURT,3786" "WEERING,3787" "WEHLA,3788" "WELLSFORD,3789" "WELSHPOOL,3790" "WELUMLA,3791" "WEMEN,3792" "WENSLEYDALE,3793" "WENTWORTH,3794" "WERMATONG,3795" "WERRAP,3796" "WERRIBEE,3797" "WERRIGAR,3798" "WERRIKOO,3799" "WERRIMULL,3800" "WESTON,3801"
"WEWIN,3802" "WEWIN,3802" "WHANREGARWEN,3803" "WHARPARILLA,3804" "WHIRILY,3805" "WHIRRAKEE,3806" "WHITFIELD,3807" "WHITFIELD SOUTH,3808" "WHOOREL,3809" "WHOROULY,3810" "WHROO,3811" "WIALL,3812" "WIBENDUCK,3813" "WICKLIFFE NORTH,3814" "WICKLIFFE SOUTH,3815" "WILGUL NORTH,3816" "WILGUL SOUTH,3817" "WILHELMINA,3818" "WILKIN,3819" "WILKUR,3820" "WILLAH,3821"
"WILLAM,3822" "WILLAM,3822" "WILLANGIE,3823" "WILLATOOK,3824" "WILLAURA,3825" "WILLENABRINA,3826" "WILLOBY,3827" "WILLOWMAVIN,3828" "WILLS,3829" "WILLUNG,3830" "WILL-WILL-ROOK,3831" "WINDARRA,3833" "WINDERMERE,3832" "WINDHAM,3834" "WING WING,3837" "WINGAN,3835" "WINGEEL,3836" "WINIAM,3838" "WINJALLOK,3839" "WINNAMBOOL,3840" "WINNINDOO,3841"
"WINTERIGA,3842" "WINTERIGA,3842" "WINTON,3843" "WINYAR,3844" "WINYAYUNG,3845" "WIRCHILLEBA,3846" "WIRIDJIL,3847" "WIRMBIRCHIP,3848" "WIRMBOOL,3849" "WIRRATE,3850" "WIRRBIBIAL,3851" "WITCHIPOOL,3852" "WOATWOARA,3853" "WODONGA,3854" "WOLLERT,3855" "WOLLONABY,3856" "WOMBAT,3857" "WOMBELANO,3858" "WONDOOMAROOK,3859" "WONGA WONGA,3862" "WONGA WONGA SOUTH,3863"
"WONGAN,3860" "WONGAN,3860" "WONGARRA,3861" "WONGUNGARRA,3864" "WONNANGATTA,3865" "WONTHAGGI,3866" "WONTHAGGI NORTH,3867" "WONWONDAH,3868" "WONWRON,3869" "WONYIP,3870" "WOODBOURNE,3871" "WOODEND,3872" "WOODNAGGERAK,3873" "WOODSIDE,3874" "WOODSTOCK,3875" "WOOHLPOOER,3876" "WOOKURKOOK,3877" "WOOLAMAI,3878" "WOOLENOOK,3879" "WOOLSTHORPE,3880" "WOOLWOOLA,3881"
"WOONGULMERANG EAST,3882" "WOONGULMERANG EAST,3882" "WOONGULMERANG WEST,3883" "WOORAK,3884" "WOORARRA,3885" "WOORI YALLOCK,3888" "WOORINEN,3886" "WOORIWYRITE,3887" "WOORNACK,3889" "WOORNDOO,3890" "WOORNYALOOK,3891" "WOORONOOK,3892" "WOORRAGEE,3893" "WOORRAGEE NORTH,3894" "WOOSANG,3895" "WOOUNDELLAH,3896" "WOOYOOT,3897" "WORAIGWORM,3898" "WORANGA,3899" "WORMANGAL,3900" "WORMBETE,3901"
"WOROOA,3902" "WOROOA,3902" "WORROUGH,3903" "WORROWING,3904" "WORTONGIE,3905" "WRATHUNG,3906" "WRIXON,3907" "WUK WUK,3908" "WULLA WULLOCK,3909" "WURDI-YOUANG,3910" "WURRIN,3911" "WURROOK,3912" "WURRUK WURRUK,3913" "WURUTWUN,3914" "WYANGIL,3915" "WYCHEPROOF,3916" "WYCHITELLA,3917" "WYEEBOO,3918" "WYELANGTA,3919" "WYMLET,3920" "WYPERFELD,3921"
"WYTWALLAN,3922" "WYTWALLAN,3922" "WYTWARRONE,3923" "WYUNA,3924" "WY-YUNG,3925" "YAAPEET,3926" "YABBA,3927" "YABBA YABBA,3928" "YACKANDANDAH,3929" "YALCA,3930" "YALIMBA,3931" "YALIMBA EAST,3932" "YALLAKAR,3933" "YALLA-Y-POORA,3934" "YALLOCK,3935" "YALLOOK,3936" "YALLUM,3937" "YALMY,3938" "YALOAK,3939" "YALONG,3940" "YALONG SOUTH,3941"
"YAMBUK,3942" "YAMBUK,3942" "YAMBULLA,3943" "YAN YAN GURT,3954" "YAN YEAN,3955" "YANAC-A-YANAC,3944" "YANAKIE,3945" "YANAKIE SOUTH,3946" "YANDOIT,3947" "YANGARDOOK,3948" "YANGERAHWILL,3949" "YANGERY,3950" "YANGOURA,3951" "YANIPY,3952" "YANNATHAN,3953" "YARAK,3956" "YARAMBA,3957" "YARCK,3958" "YARIMA,3959" "YARPTURK,3960" "YARRABERB,3961"
"YARRAGON,3962" "YARRAGON,3962" "YARRAM YARRAM,3963" "YARRAMYLJUP,3964" "YARRANGOOK,3965" "YARRARA,3966" "YARRAWONGA,3967" "YARRAYNE,3968" "YARROCK,3969" "YARROWALLA,3970" "YARROWEE,3971" "YARROWEYAH,3972" "YAT NAT,3976" "YATCHAW EAST,3973" "YATCHAW WEST,3974" "YATMERONE,3975" "YATPOOL,3977" "YAUGHER,3978" "YEA,3979" "YEARINGA,3980" "YEERIK,3981"
"YEERUNG,3982" "YEERUNG,3982" "YEHRIP,3983" "YELLANGIP,3984" "YELTA,3985" "YELWELL,3986" "YEO,3987" "YERING,3988" "YERTOO,3989" "YETH-YOUANG,3990" "YEUNGROON,3991" "YIELIMA,3992" "YINNAR,3993" "YONDUK,3994" "YOUANMITE,3995" "YOUARANG,3996" "YOUARRABUK,3997" "YOUPAYANG,3998" "YOUPELLA,3999" "YOWANG,4000" "YULECART,4001"
"YUNGERA,4002" "YUNGERA,4002" "YUONGA,4003" "YUPPECKIAR,4004" "YUROKE,4005"                
               
))



(setq townlist (list
"Aberfeldy,5001" "Acheron,5002" "Ailsa,5003" "Albacutya,5004" "Alberton,5005" "Alexandra,5006" "Allans Flat,5007" "Alma,5008" "Amherst,5009" 
"Amphitheatre,5010" "Anglesea,5011" "Annuello,5012" "Antwerp,5013" "Apollo Bay,5014" "Apsley,5015" "Arapiles,5016" "Ararat,5017" "Archdale,5018"
 "Arnold,5019" "Ascot,5020" "Aubrey,5021" "Avenel,5022" "Avoca,5023" "Axedale,5024" "Bacchus Marsh,5025" "Baddaginnie,5026" "Bairnsdale,5027"
 "Baker,5028" "Ballan,5029" "Ballarat,5030" "Ballarat East,5031" "Ballarat North,5032" "Balliang,5033" "Ballyrogan,5034" "Balmoral,5035"
 "Balnarring Beach,5036" "Bambill,5037" "Bangerang,5038" "Bannerton,5039" "Bannockburn,5040" "Banyena,5041" "Baringhup,5042" "Barkly,5043"
 "Barkstead,5044" "Barmah,5045" "Barnawartha,5046" "Barrakee,5047" "Barrapoort,5048" "Barringo,5049" "Barrys Reef,5050" "Barwon Downs,5051"
 "Barwon Heads,5052" "Bass,5053" "Bathumi,5054" "Bealiba,5055" "Bearii,5056" "Bears Lagoon,5057" "Beaufort,5058" "Beazleys Bridge,5059"
 "Beeac,5060" "Beechworth,5061" "Beenak,5062" "Beetoomba,5063" "Bellbrae,5064" "Bemm,5065" "Benalla,5066" "Benambra,5067" "Bendoc,5068"
 "Benetook,5069" "Bengworden,5070" "Benjeroop,5071" "Bennison,5072" "Berringa,5073" "Berringama,5074" "Berriwillock,5075" "Berrybank,5076"
 "Berwick,5077" "Bet Bet,5078" "Bethanga,5079" "Betley,5080" "Beulah,5081" "Beveridge,5082" "Bingo-Munjie North,5083" "Birchip,5084"
 "Birregurra,5085" "Blackwarry,5086" "Blackwood,5087" "Blakeville,5088" "Bocca Flat (see 3782,5089" "Boigbeat,5090" "Boileau,5091"
 "Boinka,5092" "Bolton,5093" "Bolwarrah,5094" "Bonang,5095" "Bonnie Doon,5096" "Boolarra,5097" "Boolite,5098" "Boonoonar,5099"
 "Boorgunyah,5100" "Booroopki,5101" "Boort,5102" "Borung,5103" "Bowenvale,5104" "Branxholme,5105" "Braybrook,5106" "Breamlea,5107"
 "Briagolong,5108" "Bridgewater,5109" "Bright,5110" "Brim,5111" "Britannia Creek,5112" "Broadford,5113" "Broadmeadows,5114" "Bromley,5115"
 "Brookville,5116" "Broomfield,5117" "Broughton,5118" "Bruarong,5119" "Bruthen,5120" "Buangor,5121" "Buchan,5122" "Buckrabanyule,5123"
 "Buffalo,5124" "Bulla,5125" "Bullarto,5126" "Bullarto South,5127" "Bullumwaal,5128" "Buln Buln,5129" "Bunbartha,5130" "Bundalong,5131"
 "Bung Bong,5132" "Bungeet,5133" "Buninyong,5134" "Bunyip,5135" "Burkes Flat,5136" "Burrereo,5137" "Burwood,5138" "Bushfield,5139" "Byaduk,5140"
 "Byaduk North,5141" "Callawadda,5142" "Callignee,5143" "Cambrian Hill,5144" "Campbells Creek,5145" "Campbelltown,5146" "Camperdown,5147"
 "Cann River,5148" "Cape Clear,5149" "Caramut,5150" "Carapooee,5151" "Carapook,5152" "Cargerie,5153" "Carisbrook,5154" "Carlsruhe,5155"
 "Carlyle,5156" "Carngham,5157" "Carrajung,5158" "Carwarp,5159" "Cashel,5160" "Cassilis,5161" "Casterton,5162" "Castlemaine,5163"
 "Castle Point,5164" "Cavendish,5165" "Charlton,5166" "Chatsworth,5167" "Chepstowe,5168" "Cherokee,5169" "Cheshunt,5170" "Chetwynd,5171"
 "Childers,5172" "Chiltern,5173" "Chinkapook,5174" "Clarendon,5175" "Clear Lake,5176" "Club Terrace,5177" "Clunes,5178" "Coalville,5179"
 "Cobden,5180" "Cobram,5181" "Coburg,5182" "Cocamba,5183" "Cockatoo,5184" "Cohuna,5185" "Colac,5186" "Colbinabbin,5187" "Coleraine,5188"
 "Comoora,5189" "Congupna Road,5190" "Cooma,5191" "Coonooer,5192" "Coopers Creek,5193" "Cope Cope,5194" "Corack,5195" "Cora Lynn,5196"
 "Corindhap,5197" "Corinella,5198" "Corop,5199" "Corryong,5200" "Costerfield,5201" "Cowa,5202" "Cowangie,5203" "Cowes,5204" "Cowwarr,5205"
 "Craigie,5206" "Cranbourne,5207" "Cravensville (see 2855,5208" "Creek View,5209" "Cressy,5210" "Creswick,5211" "Crib Point,5212"
 "Crossover,5213" "Crowlands,5214" "Cudgee,5215" "Culgoa,5216" "Cullulleraine,5217" "Cunninghame,5218" "Curyo,5219" "Dalhousie,5220"
 "Dandenong,5221" "Danyo,5222" "Dargo,5223" "Darlimurla,5224" "Darlingford,5225" "Darlington,5226" "Darnum,5227" "Darraweit Guim,5228"
 "Dartmoor,5229" "Dartmouth,5230" "Daylesford,5231" "Daylesford West,5232" "Deddick,5233" "Dederang,5234" "Dennington,5235" "Deptford,5236"
 "Dereel,5237" "Dergholm,5238" "Derrinallum,5239" "Devenish West,5240" "Devon,5241" "Diamond Creek,5242" "Diapur,5243" "Digby,5244"
 "Dimboola,5245" "Dollar,5246" "Donald,5247" "Dooen,5248" "Douglas,5249" "Drik Drik,5250" "Dromana,5251" "Drouin,5252" "Dry Diggings,5253"
 "Drysdale,5254" "Dunbulbalane,5255" "Dunkeld,5256" "Dunolly,5257" "Durham,5258" "Durham Lead,5259" "Durham Ox,5260" "East Cunninghame,5261"
 "East Murchison,5262" "Echuca,5263" "Echuca West,5264" "Eddington,5265" "Edenhope,5266" "Edi,5267" "Egerton,5268" "Eilyar,5269" "Elaine,5270"
 "Elaine North,5271" "Eldorado,5272" "Elingamite North,5273" "Ellam,5274" "Ellerslie,5275" "Elmhurst,5276" "Elmore,5277" "Elphinstone,5278"
 "Eltham,5279" "Emerald,5280" "Emu,5281" "Ensay,5282" "Epping,5283" "Epsom,5284" "Eskdale,5285" "Eurack,5286" "Euroa,5287" "Evansford,5288"
 "Everton,5289" "Fernbank,5290" "Fernihurst,5291" "Flinders,5292" "Flynn,5293" "Flynns Creek Upper,5294" "Forrest,5295" "Foster,5296"
 "Fosterville,5297" "Foxhow,5298" "Framlingham,5299" "Franklinford,5300" "Frankston,5301" "Freeburgh,5302" "Fryerstown,5303" "Furnell,5304"
 "Fyansford,5305" "Galah,5306" "Garfield,5307" "Garibaldi,5308" "Garvoc,5309" "Gavan Duffy,5310" "Geelong,5311" "Gelantipy,5312"
 "Gellibrand,5313" "Gerangamete,5314" "Gerang Gerung,5315" "Ghin Ghin,5316" "Giffard,5317" "Gipsy Point,5318" "Girgarre,5319"
 "Gisborne,5320" "Glanville,5321" "Glen Dart,5322" "Glengower,5323" "Glenlyon,5324" "Glenmaggie,5325" "Glenorchy,5326" "Glenrowen,5327"
 "Glenthompson,5328" "Glen Wills,5329" "Gobur,5330" "Golden Lake,5331" "Goldsborough,5332" "Goon Nure,5333" "Gooramadda,5334" "Goornong,5335"
 "Gooroc,5336" "Gordon,5337" "Gormandale,5338" "Goroke,5339" "Goschen,5340" "Gould,5341" "Gowar,5342" "Gowar East,5343" "Goyura,5344"
 "Granite Flat,5345" "Grant,5346" "Granton,5347" "Grantville,5348" "Granya,5349" "Graytown,5350" "Great Western,5351" "Greendale,5352"
 "Green Gully,5353" "Greens Creek,5354" "Gre Gre,5355" "Grenville,5356" "Greta,5357" "Greta West,5358" "Guildford,5359" "Gunbower,5360"
 "Gunyah Gunyah,5361" "Haddon,5362" "Haines,5363" "Hamilton,5364" "Happy Valley,5365" "Harcourt,5366" "Harrietville,5367" "Harrow,5368"
 "Hastings,5369" "Hattah,5370" "Hawkesdale,5371" "Healesville,5372" "Heathcote,5373" "Heatherlie,5374" "Hedley,5375" "Heidelberg,5376"
 "Hepburn,5377" "Hexham,5378" "Heyfield,5379" "Heywood,5380" "Hinno-Munjie,5381" "Hoddle,5382" "Hollinwood,5383" "Homebush,5384" "Hopetoun,5385"
 "Horsham,5386" "Hotspur,5387" "Howqua,5388" "Huntly,5389" "Iguana Creek,5390" "Inglewood,5391" "Inverleigh,5392" "Inverloch,5393"
"Irrewillipe,5394" "Jamieson,5395" "Jam Jerrup,5396" "Jarrott,5397" "Jeeralang Junction,5398" "Jerro,5399" "Johnsonville,5400" "Kalimna,5401"
 "Kalkallo,5402" "Kangaroo Flat,5403" "Kaniva,5404" "Karabeal,5405" "Karawinna,5406" "Kardella,5407" "Karnak,5408" "Karween,5409"
 "Katamatite,5410" "Katandra,5411" "Keilor,5412" "Kerang,5413" "Kewell,5414" "Kialla West,5415" "Kiamal,5416" "Kiata,5417" "Kilcunda,5418"
 "Kilmany,5419" "Kilmore,5420" "Kinglake Central,5421" "Kinglake East,5422" "Kingower,5423" "Kirkstall,5424" "Koetong,5425" "Kooloonong,5426"
 "Koondrook,5427" "Koonoomoo,5428" "Koonwarra,5429" "Koorooman,5430" "Kooyoora,5431" "Korokubeal,5432" "Korong Vale,5433" "Korumburra,5434"
 "Koyuga,5435" "Kulwin,5436" "Kurraca,5437" "Kyabram,5438" "Kyneton,5439" "Laanecoorie,5440" "Laang,5441" "Lah,5442" "Lake Boga,5443"
 "Lake Bolac,5444" "Lake Charm,5445" "Lake Rowan,5446" "Lakes Entrance,5447" "Lal Lal,5448" "Lamplough,5449" "Lancefield,5450"
 "Landsborough,5451" "Lara,5452" "Launching Place,5453" "Lauriston,5454" "Lawloit,5455" "Lawrence,5456" "Learmonth,5457" "Leichardt,5458"
 "Leonards Hill,5459" "Leongatha,5460" "Lethbridge,5461" "Lexton,5462" "Lillimur,5463" "Lillimur South,5464" "Lilydale,5465" "Linga,5466"
 "Linton,5467" "Lismore,5468" "Little River,5469" "Llanelly,5470" "Lockington,5471" "Locksley,5472" "Lockwood,5473" "Longerenong,5474"
 "Longford,5475" "Longwarry,5476" "Longwood,5477" "Lorne,5478" "Lorquon,5479" "Lower Emu,5480" "Lower Homebush,5481" "Lubeck,5482"
 "Lucknow,5483" "Lyons,5484" "Lyonville,5485" "Macarthur,5486" "Macedon,5487" "Mackinnons Bridge,5488" "Mafeking,5489" "Maffra,5490"
 "Maindample,5491" "Majorca,5492" "Maldon,5493" "Mallacoota,5494" "Malmsbury,5495" "Manangatang,5496" "Mandurang,5497" "Mangalore,5498"
 "Manorina,5499" "Mansfield,5500" "Marengo,5501" "Maribyrnong,5502" "Marlo,5503" "Marnoo,5504" "Marong,5505" "Maroona,5506" "Marungi,5507"
 "Maryborough,5508" "Marysville,5509" "Mathiesons,5510" "Matlock,5511" "Maude,5512" "Meeniyan,5513" "Melbourne,5514" "Melbourne,5514A"
 "Melbourne,5514A" "Melbourne,5514B" "Melbourne,5514B" "Melbourne,5514C" "Melbourne,5514D" "Melton,5515" "Merbein,5516" "Meredith,5517"
 "Meringo,5518" "Meringur,5519" "Merino,5520" "Merricks,5521" "Merrijig,5522" "Merrinee,5523" "Merton,5524" "Metcalfe,5525" "Metung,5526"
 "Mia Mia,5527" "Middle Creek,5528" "Miepoll,5529" "Milltown,5530" "Miners Rest,5531" "Minimay,5532" "Minyip,5533" "Miralie,5534" "Miram,5535"
 "Mirboo,5536" "Mirboo North,5537" "Mitchellstown,5538" "Mitta Mitta,5539" "Mittyack,5540" "Modewarre,5541" "Moe,5542" "Molesworth,5543"
 "Moliagul,5544" "Molyullah,5545" "Monbulk,5546" "Moonambel,5547" "Moorilim,5548" "Mooroopna,5549" "Morkalla,5550" "Mornington,5551"
"Morrisons,5552" "Mortlake,5553" "Morwell,5554" "Mossiface,5555" "Moyston,5556" "Muckatah,5557" "Mudgeegonga,5558" "Mumbannar,5559"
 "Munro,5560" "Murchison,5561" "Murrabit,5562" "Murra Warra,5563" "Murrayville,5564" "Murrungowar,5565" "Murtoa,5566" "Myrniong,5567"
 "Myrtleford,5568" "Mystic Park,5569" "Nagambie,5570" "Nalinga,5571" "Nandaly,5572" "Napoleons,5573" "Narbethong,5574" "Nariel,5575"
"Narrawong,5576" "Nathalia,5577" "Natimuk,5578" "Natte Yallock,5579" "Natya,5580" "Navarre,5581" "Neerim,5582" "Neilborough,5583" "Nelson,5584"
 "Nerrina,5585" "Netherby,5586" "Neuarpur,5587" "Newbridge,5588" "Newbury,5589" "Newhaven,5590" "Newlyn North,5591" "Newmerella,5592"
 "Newry,5593" "Newstead,5594" "Nhill,5595" "Nilma,5596" "Ni Ni,5597" "Ninyeunook,5598" "Nirranda,5599" "Noojee,5600" "Noradjuha,5601"
"Norong,5602" "Northcote,5603" "Nowa Nowa,5604" "Nowingi,5605" "Nullawil,5606" "Numurkah,5607" "Nungurner,5608" "Nuntin,5609" "Nurrabiel,5610"
"Nyah,5611" "Nyah West,5612" "Nyora,5613" "Oakleigh,5614" "Old Longwood,5615" "Olinda,5616" "Omeo,5617" "Orbost,5618" "Orford,5619"
 "Osborne,5620" "Ouyen,5621" "Oxley,5622" "Pakenham,5623" "Panitya,5624" "Panmure,5625" "Panton Hill,5626" "Paynesville,5627" "Peechelba,5628"
 "Penshurst,5629" "Percydale,5630" "Peterborough,5631" "Pheasant Creek,5632" "Piangil,5633" "Pier-Millan,5634" "Pigeon Ponds,5635"
 "Pimpinio,5636" "Pira,5637" "Piries,5638" "Pirlta,5639" "Pirron Yallock,5640" "Pitfield,5641" "Pitfield Plains,5642" "Pollard,5643"
 "Poowong,5644" "Porepunkah,5645" "Port Albert,5646" "Portarlington,5647" "Port Campbell,5648" "Port Fairy,5649" "Port Franklin,5650"
 "Portland,5651" "Port Welshpool,5652" "Powelltown,5653" "Princetown,5654" "Pullut,5655" "Pura Pura,5656" "Purdeet,5657" "Pyalong,5658"
 "Pyramid Hill,5659" "Quambatook,5660" "Queenscliff,5661" "Queenstown,5662" "Raglan,5663" "Rainbow,5664" "Ravenswood,5665" "Raymond Island,5666"
 "Raywood,5667" "Redbank,5668" "Redcastle,5669" "Redesdale,5670" "Red Hill South,5671" "Reedy Creek,5672" "Rheola,5673" "Rhyll,5674"
 "Riddell,5675" "Ringwood,5676" "Robinvale,5677" "Rochester,5678" "Rokeby,5679" "Rokewood,5680" "Romsey,5681" "Rosebud,5682" "Rosedale,5683"
 "Rossbridge,5684" "Rowsley,5685" "Ruffy,5686" "Runnymede,5687" "Rupanyup,5688" "Rushworth,5689" "Rutherglen,5690" "Rye,5691" "St. Arnaud,5692"
 "St. Clair,5693" "St. Leonards,5694" "Sale,5695" "Salisbury,5696" "Sandford,5697" "Sandy Point,5698" "San Remo,5699" "Sarsfield,5700"
 "Scotts Creek,5701" "Seacombe,5702" "Sea Lake,5703" "Seaspray,5704" "Seaton,5705" "Sebastian,5706" "Sebastopol,5707" "Serpentine,5708"
 "Serviceton,5709" "Seville,5710" "Seymour,5711" "Shelford,5712" "Shepparton,5713" "Shirley,5714" "Shoreham,5715" "Skenes Creek,5716"
 "Skipton,5717" "Skye,5718" "Smeaton,5719" "Smiths Gully,5720" "Smythesdale,5721" "Sorrento,5722" "South Bannockburn,5723"
 "South Muckleford,5724" "Speed,5725" "Spring Hill,5726" "Springhurst,5727" "Stanhope,5728" "Stanley,5729" "Stawell,5730" "Steiglitz,5731"
 "Stirling,5732" "Stockyard Hill,5733" "Stony Creek,5734" "Stradbroke,5735" "Stratford,5736" "Strathallan,5737" "Strathbogie,5738"
 "Strath Creek,5739" "Strathfieldsaye,5740" "Streatham,5741" "Stuartmill,5742" "Suggan Buggan,5743" "Sunbury,5744" "Sunnyside,5745"
 "Sutton Grange,5746" "Swan Hill,5747" "Swanpool,5748" "Swan Reach,5749" "Swifts Creek,5750" "Sydenham,5751" "Tabbara,5752" "Taggerty,5753"
 "Tahara,5754" "Talbot,5755" "Tallangallook,5756" "Tallangatta,5757" "Tallangatta Valley,5758" "Tallarook,5759" "Tamboon,5760"
 "Tamboon South,5761" "Taradale,5762" "Tarilta,5763" "Tarkedia,5764" "Tarnagulla,5765" "Tarranginnie,5766" "Tarraville,5767"
 "Tarrayoukyan,5768" "Tarwin,5769" "Tarwin Lower,5770" "Tatong,5771" "Tatonga,5772" "Tatura,5773" "Teesdale,5774" "Telopea Downs,5775"
 "Templestowe,5776" "Tempy,5777" "Terang,5778" "Terrick Terrick,5779" "Terrick Terrick South,5780" "The Gap,5781" "Thoona,5782"
 "Timboon,5783" "Timor,5784" "Tolmie,5785" "Tongala,5786" "Tongio-Munjie,5787" "Tongio West,5788" "Toolamba,5789" "Toolangi,5790"
 "Toolern Vale,5791" "Toolleen,5792" "Toolondo,5793" "Toombon,5794" "Toongabbie,5795" "Torquay,5796" "Torrita,5797" "Towaninny,5798"
 "Towong,5799" "Trafalgar,5800" "Traralgon,5801" "Trentham,5802" "Trinita,5803" "Tungamah,5804" "Tunstals,5805" "Tutye,5806"
 "Tyaak,5807" "Tyers (see 2185 Boola,5808" "Tylden,5809" "Tynong,5810" "Tyrendarra,5811" "Underbool,5812" "Vaughan,5813" "Ventnor,5814"
 "Violet Town,5815" "Waaia,5816" "Waanyarra,5817" "Wail,5818" "Walhalla,5819" "Walkerville,5820" "Wallace,5821" "Wallan,5822" "Walmer,5823"
 "Walpeup,5824" "Wal Wal,5825" "Wandiligong,5826" "Wandin Yallock,5827" "Wando Vale,5828" "Wangaratta,5829" "Wannon,5830" "Warburton,5831"
 "Wareek,5832" "Warneet,5833" "Warracknabeal,5834" "Warragul,5835" "Warrak,5836" "Warrandyte,5837" "Warrandyte North" "Warrayure,5839"
 "Warrenheip,5840" "Warrnambool,5841" "Watchem,5842" "Waubra,5843" "Waygara,5844" "Wedderburn,5845" "Wehla,5846" "Werribee,5847"
 "Werrimull,5848" "Wesburn,5849" "Westbury,5850" "Westmere,5851" "Wharparilla North,5852" "Whiskey Creek,5853" "White Hills,5854"
 "Whittlesea,5855" "Whorouly,5856" "Whroo,5857" "Wickliffe,5858" "Wilby,5859" "Willenabrina,5860" "Williamstown,5861" "Willow Grove,5862"
 "Willung,5863" "Winchelsea,5864" "Wingeel,5865" "Winslow,5866" "Winton,5867" "Winyar,5868" "Wodonga,5869" "Wombelano,5870" "Wonthaggi,5871"
 "Wonwondah East,5872" "Wonwondah North,5873" "Woodend,5874" "Woodford,5875" "Woodside,5876" "Woodside North,5877" "Woods Point,5878"
 "Wood Wood,5879" "Woolamai,5880" "Woolsthorpe,5881" "Woomelang,5882" "Woorinen South,5883" "Woorndoo,5884" "Wooroonook,5885" "Wunghnu,5886"
 "Wurdi Boluc,5887" "Wurruk,5888" "Wycheproof,5889" "Wyelangta,5890" "Wyuna,5891" "Yaapeet,5892" "Yackandandah,5893" "Yambuk,5894"
 "Yanac South,5895" "Yandoit,5896" "Yarck,5897" "Yarragon,5898" "Yarra Junction,5899" "Yarrara,5900" "Yarrawonga,5901" "Yarto,5902"
 "Yatpool,5903" "Yea,5904" "Yellingbo,5905" "Yelta,5906" "Yendon,5907" "Yungera,5908" "Yuppeckiar,5909"
))



(setq lgalist (list
"ALPINE SHIRE,300" "ALPINE SHIRE,300" "ARARAT RURAL CITY,301" "BALLARAT CITY,302" "BANYULE CITY,303" "BASS COAST SHIRE,304" "BAW BAW SHIRE,305" "BAYSIDE CITY,306" "BENALLA RURAL CITY,381" "BOROONDARA CITY,307" "BRIMBANK CITY,308" "BULOKE SHIRE,309" "CAMPASPE SHIRE,310" "CARDINIA SHIRE,311" "CASEY CITY,312" "CENTRAL GOLDFIELDS SHIRE,313" "COLAC OTWAY SHIRE,314" "CORANGAMITE SHIRE,315" "DAREBIN CITY,316" "EAST GIPPSLAND SHIRE,319" "FALLS CREEK ALPINE RESORT (UNINCORPORATED),386"
"FRANKSTON CITY,320" "FRANKSTON CITY,320" "FRENCH ISLAND (UNINCORPORATED),379" "GANNAWARRA SHIRE,321" "GLEN EIRA CITY,322" "GLENELG SHIRE,323" "GOLDEN PLAINS SHIRE,324" "GREATER BENDIGO CITY,325" "GREATER DANDENONG CITY,326" "GREATER GEELONG CITY,327" "GREATER SHEPPARTON CITY,328" "HEPBURN SHIRE,329" "HINDMARSH SHIRE,330" "HOBSONS BAY CITY,331" "HORSHAM RURAL CITY,332" "HUME CITY,333" "INDIGO SHIRE,334" "KINGSTON CITY,335" "KNOX CITY,336" "LAKE MOUNTAIN ALPINE RESORT (UNINCORPORATED),385" "LATROBE CITY,337"
"LODDON SHIRE,338" "LODDON SHIRE,338" "MACEDON RANGES SHIRE,339" "MANNINGHAM CITY,340" "MANSFIELD SHIRE,382" "MARIBYRNONG CITY,341" "MAROONDAH CITY,342" "MELBOURNE CITY,343" "MELTON CITY,344" "MILDURA RURAL CITY,345" "MITCHELL SHIRE,346" "MOIRA SHIRE,347" "MONASH CITY,348" "MOONEE VALLEY CITY,349" "MOORABOOL SHIRE,350" "MORELAND CITY,351" "MORNINGTON PENINSULA SHIRE,352" "MOUNT ALEXANDER SHIRE,353" "MOUNT BAW BAW ALPINE RESORT (UNINCORPORATED),383" "MOUNT BULLER ALPINE RESORT (UNINCORPORATED),384" "MOUNT HOTHAM ALPINE RESORT (UNINCORPORATED),388"
"MOUNT STIRLING ALPINE RESORT (UNINCORPORATED),387" "MOUNT STIRLING ALPINE RESORT (UNINCORPORATED),387" "MOYNE SHIRE,354" "MURRINDINDI SHIRE,355" "NILLUMBIK SHIRE,356" "NORTHERN GRAMPIANS SHIRE,357" "PORT PHILLIP CITY,358" "PYRENEES SHIRE,359" "QUEENSCLIFFE BOROUGH,360" "SOUTH GIPPSLAND SHIRE,361" "SOUTHERN GRAMPIANS SHIRE,362" "STONNINGTON CITY,363" "STRATHBOGIE SHIRE,364" "SURF COAST SHIRE,365" "SWAN HILL RURAL CITY,366" "TOWONG SHIRE,367" "WANGARATTA RURAL CITY,368" "WARRNAMBOOL CITY,369" "WELLINGTON SHIRE,370" "WEST WIMMERA SHIRE,371" "WHITEHORSE CITY,372"
"WHITTLESEA CITY,373" "WHITTLESEA CITY,373" "WODONGA CITY,374" "WYNDHAM CITY,375" "YARRA CITY,376" "YARRA RANGES SHIRE,377" "YARRIAMBIACK SHIRE,378"              
))



(defun linebuilder (/)

 ;----------------------------------------start line builder

  
  ;get line 1

  

 

(if (= (substr sobs 1 3) "arc")(progn

				 ;seperate obs and points
       (setq !pos1 (vl-string-position 33 sobs 0))
  (setq ~pos1 (vl-string-position 126 sobs 0))
       (setq ,pos1 (vl-string-position 44 sobs 0))
            
   (setq bearing  (substr sobs 4 (- ~pos1 3)))
   (setq arclength (substr sobs (+ ~pos1 2) (- (- ,pos1 ~pos1) 1)))
       (setq radius (substr sobs (+ ,pos1 2) (- (- !pos1 ,pos1) 1)))
   (setq target (substr sobs  (+ !pos1 2) 200))
				  
  ;calc chord distance, note using string values not digital values
	    (setq stringO (/ (atof arclength) (atof radius)));arc internal angle based on string values
	    (setq dist (rtos (* 2 (atof radius) (sin (/ stringO 2))) 2 3))
  )

 
  
(progn ; is a line
  ;seperate obs and points
       (setq !pos1 (vl-string-position 33 sobs 0))
  (setq ~pos1 (vl-string-position 126 sobs 0))
   (setq bearing  (substr sobs 1 ~pos1))
   (setq dist (substr sobs (+ ~pos1 2) (+ !pos1 2)))
   (setq target (substr sobs  (+ !pos1 2) 200))
));p& ifline/arc

  ;APPLY ALL CORRECTIONS AND EXTRACT INFORMATION FROM USER INPUT
(if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
  (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins  (substr bearing (+ dotpt1 2) 2))
  (setq sec  (substr bearing (+ dotpt1 4) 10))

  ;truncated mins and secs
  (if (= (strlen sec) 1)  (setq sec (strcat sec "0" )))
  (if (= (strlen mins) 1)  (setq mins (strcat mins "0" )))
  (setq mins (strcat mins (chr 39)))
  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))

  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))
  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

  ;look for R
  (if (or (= (substr bearing 1 1 ) "r") (= (substr bearing 1 1 ) "R" ))
    (progn
      (setq deg (substr deg 2 10))
      (setq rswitch "T")
      )
    (setq rswitch "F")
    )
  
        
	      

    ;DRAW LINE 1
      (setq lbearing bearing)
  ;(IF ( = rswitch "T")(setq obearing (substr lbearing 2 200))(setq obearing lbearing))
  (setq dist (rtos (atof dist)2 3));remove trailing zeros
  
  (setq ldist dist)
  (setq bearing (strcat  deg "d" mins sec))

  (setq linetext (strcat "@" dist "<" bearing))
    (command "line" startpoint linetext "")

  (setq sent (entlast))
   (SETQ SENTLIST (ENTGET SENT))

    (setq p2 (CDR(ASSOC 11 sentlist)))
    
  
;Move line if reverse activated
(if (= rswitch "T")
  (progn
    (setq p1 (CDR(ASSOC 10 sentlist)))
    (setq p2 (CDR(ASSOC 11 sentlist)))
    (command "move" sent "" p2 p1)
     (setq sent (entlast))
   (SETQ SENTLIST (ENTGET SENT))
    (setq p2 (CDR(ASSOC 10 sentlist)))
    
 
    )
  )

  ;(command "erase" sent "")
  (setq dellines (ssadd sent dellines))
    
 ; (alert (strcat "\n" sp " to " target " oc" (rtos obscount 2 0) "-" (rtos (length orderlist) 2 0) "-" (rtos (length cgpointnum) 2 0)))

  (if (= (setq remlist (member target cgpointnum)) nil)
    (progn;if not in list
      (setq cgpointnum (append cgpointnum (list target)))
      (setq cgpointco (append cgpointco (list p2)))
      (setq networklist (append networklist (list (strcat nwls "," target))))
      
(setq orderlist (append orderlist (list order)))
      
      (setq remlist (member target cgpointnum))
       (setq existco (nth (- (length cgpointnum)(length remlist)) cgpointco))
      )
    (progn;if in list
      (setq existco (nth (- (length cgpointnum)(length remlist)) cgpointco))
      (setq existnwls (nth (- (length cgpointnum)(length remlist)) networklist))

      (setq newnwls (strcat nwls "," target))
      (princ (strcat "\nAt "target))
           (princ (strcat "\nExisting loop - " existnwls))
      (princ (strcat "\nClosing loop - " newnwls))
      

;parse csv string to list
      (setq newlist nil)
      (setq existlist nil)
      
      (while (/= (setq ,pos (vl-string-position 44 newnwls 0)) nil)
      (progn
	(setq newlist (append newlist (list (substr newnwls 1 ,pos))))
	(setq newnwls (substr newnwls (+ ,pos 2) ))
))
      (setq newlist (append newlist (list newnwls)))
        (while (/= (setq ,pos (vl-string-position 44 existnwls 0)) nil)
      (progn
	(setq existlist (append existlist (list (substr existnwls 1 ,pos))))
	(setq existnwls (substr existnwls (+ ,pos 2)))
))
      (setq existlist (append existlist (list existnwls)))
      
      (setq newlist (reverse newlist))
      (setq existlist (reverse existlist))

      (setq nlcount 1)
      (setq distcov 0)
      (setq prevpt target)
      (setq n 1)
      ;search through list adding setup and distances until you find a common point
      (while (= (setq exremlist (member (nth nlcount newlist) existlist)) nil)
	(progn
	  (setq frompt (nth (- (length cgpointco)(length (member prevpt cgpointnum)))cgpointco))
	  (setq topt (nth (- (length cgpointco)(length (member (nth nlcount newlist) cgpointnum))) cgpointco))
	  (setq distcov (+ distcov(distance frompt topt)))
	;	(princ (strcat "\nFrom " prevpt " " (rtos (car frompt) 2 3)","(rtos (cadr frompt) 2 3)))
	;	 (princ (strcat "\nto " (nth nlcount newlist) (rtos (car topt) 2 3)","(rtos (cadr topt) 2 3)))

	 ; (princ (strcat "\n NDistcov" (rtos distcov 2 3)))
	  (setq n (+ n 1))
	  (setq prevpt (nth nlcount newlist))
	  (setq nlcount (+ nlcount 1))
	  ))
      
	
	  (setq frompt (nth (- (length cgpointco)(length (member prevpt cgpointnum)))cgpointco))
	  (setq topt (nth (- (length cgpointco)(length (member (nth nlcount newlist) cgpointnum))) cgpointco))
	  (setq distcov (+ distcov (distance frompt topt)))
	;	(princ (strcat "\nFrom " prevpt " " (rtos (car frompt) 2 3)","(rtos (cadr frompt) 2 3)))
	;	 (princ (strcat "\nto " (nth nlcount newlist) (rtos (car topt) 2 3)","(rtos (cadr topt) 2 3)))

	 ; (princ (strcat "\n NDistcov" (rtos distcov 2 3)))

      (setq numexlist (+ (- (length existlist)(length exremlist)) 1))
      ;(princ (strcat "\n Numexlist - " (rtos numexlist 2 0)))
      (setq felist nil)
      (setq nxlcount 0)
      (repeat numexlist
	(setq felist (append felist (list (nth nxlcount existlist))))
	(setq nxlcount (+ nxlcount 1))
	)
      

      
     	  (setq excount 1)
      (setq prevpt target)
            ;repeat for exist list after finding common point
      (repeat (- (length felist) 1)
	(setq frompt (nth (- (length cgpointco)(length (member prevpt cgpointnum)))cgpointco))
	  (setq topt (nth (- (length cgpointco)(length (member (nth excount felist) cgpointnum))) cgpointco))
	(setq distcov (+ distcov (distance frompt topt)))
;	(princ (strcat "\nFrom " prevpt " " (rtos (car frompt) 2 3)","(rtos (cadr frompt) 2 3)))
;		 (princ (strcat "\nto " (nth excount felist) (rtos (car topt) 2 3)","(rtos (cadr topt) 2 3)))
;(princ (strcat "\n EDistcov" (rtos distcov 2 3)))
	(setq n (+ n 1))
	(setq prevpt (nth excount existlist))
	(setq excount (+ excount 1))
	)

                 
 
      (princ (strcat "\nDistance Covered - " (rtos distcov 2 3) "    Measured misclose- " (rtos (distance existco p2) 2 3)))
      (princ (strcat "\nNumber of setups - " (rtos n 2 0)))

       (if (> (distance existco p2) largemiss )(setq largemiss (distance existco p2)))

      (if (= xlcchecker "Y")(progn
			      
      (setq angallow (+ (atof isdb) (* (atof isdb) (sqrt n))))
       (setq pluspos (vl-string-position 43 isdd 0))
      (setq sddc (atof (substr isdd 1 pluspos)))
      (setq sddp (atof (substr isdd (+ pluspos 2) 50)))

      
      ;calc standard deviation of distance
      (setq sddd  (/ (+ sddc (* (/  distcov 1000) sddp)) 1000))

      (setq allowmiss (+ (cadr (polar (list 0 0) (* (/ angallow 3600) (/ pi 180)) distcov))  (/ sddc 1000)  sddd ))
      (princ (strcat "\nAllowable misclose -" (rtos allowmiss 2 3)))
      (princ (strcat "\n Measured misclose- " (rtos (distance existco p2) 2 3)))
      (if (> (distance existco p2) allowmiss)
	(PROGN
	  (princ "\nFAILED")
	  (IF (= XLCEXPORT "Y")(WRITE-LINE (STRCAT SHNAME "," (RTOS DISTCOV 2 3) "," (RTOS N 2 0) "," (RTOS ALLOWMISS 2 3) "," (RTOS (distance existco p2) 2 3)) OUTFILE))
	  ));IF FAILED
	     
      ));IF XLCCHECKER
      
      (if (>  (distance existco p2)  0.001)
	(progn

	  
	  (command "layer" "m" "Miscloses" "c" "Red" "Miscloses" "" )
      (command "line" existco p2 "")
	  (setq sent (entlast))
      (command "scale" sent "" p2 "100")
	  (setq mdist (rtos (* (distance existco p2 ) 1000) 2 0))
	  (setq mang (angtos (angle p2 existco) 0 0))
	  (COMMAND "TEXT" "J" "BL" existco TH (angtos (angle p2 existco) 0 5) (strcat mang "°-" mdist ))

	  (setq curorder (nth (- (length cgpointnum)(length remlist)) orderlist))
	 

	  (if (< order curorder);if new point is higher in the heirachy
	    (progn
	      
	      (setq cgpointnum (remove_nth cgpointnum   (- (length cgpointnum)(length remlist))))
	      (setq cgpointco (remove_nth cgpointco   (- (length cgpointco)(length remlist))))
	      (setq orderlist (remove_nth orderlist   (- (length orderlist)(length remlist))))
	      (setq networklist (remove_nth networklist (- (length networklist)(length remlist))))
	      (setq cgpointnum (append cgpointnum (list target)))
              (setq cgpointco (append cgpointco (list p2)))
              (setq orderlist (append orderlist (list order)))
	      (setq networklist (append networklist (list (strcat nwls "," target))))
	      ;  (alert (strcat "\n" sp " to " target " oc" (rtos obscount 2 0) "-" (rtos (length orderlist) 2 0) "-" (rtos (length cgpointnum) 2 0)))

	      )
	    )
	  
      ));dist > 0
      )
    )

  
)
  

(defun obsimporter (/)
   (setq prevlayer (getvar "CLAYER"))
  (SETVAR "CLAYER"  "Lot Definitions" )
 
  (setq normalpoints nil)
  (setq normalobs nil)
  (setq travpoints nil)
  (setq travobs nil)
  (setq topopoints nil)
  (setq topoobs nil)
  (setq sidepoints nil)
  (setq sideobs nil)
  (setq cgpointnum (list));list of coordinate geometery names
  (setq cgpointco (list)) ;listof coordunate geometery coorindates
  (setq norder (list));lists for order of insertion of lines
  (setq networklist (list));list of points to get to position
  (setq toorder (list))
  (setq torder (list))
  (setq sorder (list))
  (setq orderlist (list))
  (setq origcgpointlist (list));original coordintes for establishing shifts
  (setq newcgpointlist (list));new coorinates of the same points
  (setq dellines (ssadd))
  (setq largemiss 0)

  (setq cgpointlist (list));list of names coordinates and types for importer
  
  ;(setq xmlfilen (getfiled "Select XML file" "" "xml" 2))


`
  
  

  ;linefeed to observations
(setq linetext "")
  (setq obscount 0)

  (IF (AND (/= XLCCHECKER "D")(/= XLCCHECKER "Y"))(PROGN;IF IMPRTING FROM XML FILE
			   
   (while (= (vl-string-search "<ObservationGroup" linetext) nil) ( progn
  (linereader)
))
   
   )
    
    )


  (setq rocount 0)
  (IF (AND (/= XLCCHECKER "D")(/= XLCCHECKER "Y"))  (linereader)(setq linetext (nth rocount rolist)
					     rocount (+ rocount 1)))
  
(while (= (vl-string-search "</ObservationGroup>" linetext ) nil)( progn

								   (setq rmline 0);reset trigger for rmline with monument at other end.

		;line observation--------------------------------------------------------------------------------------------------
		(if (/= (vl-string-search "<ReducedObservation" linetext ) nil)
	    (progn
	      
	      
	    (if (setq stringpos (vl-string-search "desc" linetext ))(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
            (setq rodesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq rodesc ""))

	    (setq stringpos (vl-string-search "purpose" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
            (setq purpose (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))))
	    (setq rolayer purpose)

	    (setq stringpos (vl-string-search "setupID" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
            (setq setupid  (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))))
	    
	    (setq stringpos (vl-string-search "targetSetupID" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
            (setq targetid  (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14))))
	    
	    (setq stringpos (vl-string-search "azimuth" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
            (setq bearing (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))))
	    (setq xbearing bearing)

	    (if (/= (setq stringpos (vl-string-search "horizDistance" linetext )) nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
            (setq dist (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq dist ""))

	    (if (/= (setq stringpos (vl-string-search "distanceType=" linetext ))nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 14)))
            (setq distancetype (strcat " distanceType=\"" (substr linetext (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13))) "\" ")))(setq distancetype ""))

	    (if (/= (setq stringpos (vl-string-search "distanceAdoptionFactor" linetext )) nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 24)))
            (setq daf (substr linetext (+ stringpos 25) (-(- wwpos 1)(+ stringpos 23)))))(setq daf ""))

	    

	    	      (if (/= (setq stringpos (vl-string-search "<FieldNote>" linetext )) nil)(progn
											   
(if (setq wwpos (vl-string-position 34 linetext (+ stringpos 12)))
  (progn;if field not contains ""s
    (setq comment (substr linetext (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11))))
    )
  (progn; else use the < to get other end of field note
    (setq <pos (vl-string-position 60 linetext (+ stringpos 11)))
    (setq comment (substr linetext (+ stringpos 12) (-(- <pos 1)(+ stringpos 10))))
    )
  )
)
  (setq comment ""))
(setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" comment &pos )) nil) (setq comment (vl-string-subst "&" "&amp;"  comment &pos)
										      &pos (+ &pos 1)))
	    
(if (/= dist "")(progn
	    (if (= rolayer "normal")(progn
				    (if (= (member (strcat bearing "~" dist "!" targetid) normalobs) nil)(progn
				    
	      (setq normalpoints (append normalpoints (list setupid)))
	      (setq normalobs (append normalobs (list (strcat bearing "~" dist "!" targetid))))
	      (setq normalpoints (append normalpoints (list targetid)))
	      (setq normalobs (append normalobs (list (strcat "r" bearing "~" dist "!" setupid))))
	      ));p&if not already in list
				    ));p&if normal layer

	    (if (= rolayer "topo")(progn
				    (if (= (member (strcat bearing "~" dist "!" targetid) topoobs) nil)(progn
				    
	      (setq topopoints (append topopoints (list setupid)))
	      (setq topoobs (append topoobs (list (strcat bearing "~" dist "!" targetid))))
	      (setq topopoints (append topopoints (list targetid)))
	      (setq topoobs (append topoobs (list (strcat "r" bearing "~" dist "!" setupid))))
	      ));p&if not already in list
				    ));p&if boundary layer

	    (if (= rolayer "traverse")(progn
				    (if (= (member (strcat bearing "~" dist "!" targetid) travobs) nil)(progn
				    
	      (setq travpoints (append travpoints (list setupid)))
	      (setq travobs (append travobs (list (strcat bearing "~" dist "!" targetid))))
	      (setq travpoints (append travpoints (list targetid)))
	      (setq travobs (append travobs (list (strcat "r" bearing "~" dist "!" setupid))))
	      ));p&if not already in list
				    ));p&if traverse layer
	      
	      (if (= rolayer "sideshot")(progn
				    (if (= (member (strcat bearing "~" dist "!" targetid) sideobs) nil)(progn
				    
	      (setq sidepoints (append sidepoints (list setupid)))
	      (setq sideobs (append sideobs (list (strcat bearing "~" dist "!" targetid))))
	      (setq sidepoints (append sidepoints (list targetid)))
	      (setq sideobs (append sideobs (list (strcat "r" bearing "~" dist "!" setupid))))
	      ));p&if not already in list
				    ));p&if sideshot layer
	    
	    
	    
	    (setq obscount (+ obscount 1))
	    ));if dist is not ""


	    	    ));pif line


;------------arc observation-------------------------------------------------------------------------------------------------


		(if (/= (vl-string-search "<ReducedArcObservation" linetext ) nil)
	    (progn
	      
	      
	    (if (setq stringpos (vl-string-search "desc" linetext ))(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))
            (setq rodesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq rodesc ""))

	    (setq stringpos (vl-string-search "purpose" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
            (setq purpose (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))))
	    (setq rolayer purpose)

	    (setq stringpos (vl-string-search "setupID" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
            (setq setupid  (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))))
	    
	    (setq stringpos (vl-string-search "targetSetupID" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 15)))
            (setq targetid (substr linetext (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14))))
	    
	    (setq stringpos (vl-string-search "chordAzimuth" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 14)))
            (setq bearing (substr linetext (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13))))
	    (setq xbearing bearing)

	    (setq stringpos (vl-string-search "length" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
            (setq arclength (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
	    (setq arclength (rtos (atof arclength)2 3));remove trailing zeros

	    (setq stringpos (vl-string-search "radius" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 8)))
            (setq radius (substr linetext (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
	    

	    (setq stringpos (vl-string-search "rot" linetext ))
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 5)))
            (setq curverot (substr linetext (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4))))

	    (if (/= (setq stringpos (vl-string-search "arcType" linetext )) nil)(progn
	    (setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))
            (setq arcType (strcat " arcType=\"" (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))) "\"")))(setq arcType ""))

	   	      (if (/= (setq stringpos (vl-string-search "<FieldNote>" linetext )) nil)(progn
											   
(if (setq wwpos (vl-string-position 34 linetext (+ stringpos 12)))
  (progn;if field not contains ""s
    (setq comment (substr linetext (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11))))
    )
  (progn; else use the < to get other end of field note
    (setq <pos (vl-string-position 60 linetext (+ stringpos 11)))
    (setq comment (substr linetext (+ stringpos 12) (-(- <pos 1)(+ stringpos 10))))
    )
  )
)
  (setq comment ""))
	    
	    (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" comment &pos )) nil) (setq comment (vl-string-subst "&" "&amp;"  comment &pos)
										      &pos (+ &pos 1)))



(if (= rolayer "normal")(progn
				    (if (= (member (strcat "arc" bearing "~" arclength "," radius "!" targetid) Normalobs) nil)(progn
				    
	      (setq Normalpoints (append Normalpoints (list setupid)))
	      (setq Normalobs (append Normalobs (list (strcat "arc" bearing "~" arclength "," radius "!" targetid))))
	      (setq Normalpoints (append Normalpoints (list targetid)))
	      (setq Normalobs (append Normalobs (list (strcat "arc" "r" bearing "~" arclength "," radius "!" setupid))))
	      ));p&if not already in list
				    ));p&if Normal layer

	    (if (= rolayer "topo")(progn
				    (if (= (member (strcat "arc" bearing "~" arclength "," radius "!" targetid) topoobs) nil)(progn
				    
	      (setq topopoints (append topopoints (list setupid)))
	      (setq topoobs (append topoobs (list  (strcat "arc" bearing "~" arclength "," radius "!" targetid))))
	      (setq topopoints (append topopoints (list targetid)))
	      (setq topoobs (append topoobs (list (strcat "arc" "r" bearing "~" arclength "," radius "!" setupid))))
	      ));p&if not already in list
				    ));p&if topo layer

	    (if (= rolayer "traverse")(progn
				    (if (= (member (strcat "arc" bearing "~" arclength "," radius "!" targetid) travobs) nil)(progn
				    
	      (setq travpoints (append travpoints (list setupid)))
	      (setq travobs (append travobs (list  (strcat "arc" bearing "~" arclength "," radius "!" targetid))))
	      (setq travpoints (append travpoints (list targetid)))
	      (setq travobs (append travobs (list (strcat "arc" "r" bearing "~" arclength "," radius "!" setupid))))
	      ));p&if not already in list
				    ));p&if traverse layer
	    
	     (if (= rolayer "sideshot")(progn
				    (if (= (member (strcat "arc" bearing "~" arclength "," radius "!" targetid) sideobs) nil)(progn
				    
	      (setq sidepoints (append sidepoints (list setupid)))
	      (setq sideobs (append sideobs (list  (strcat "arc" bearing "~" arclength "," radius "!" targetid))))
	      (setq sidepoints (append sidepoints (list targetid)))
	      (setq sideobs (append sideobs (list (strcat "arc" "r" bearing "~" arclength "," radius "!" setupid))))
	      ));p&if not already in list
				    ));p&if sideshot layer
	    
	 (setq obscount (+ obscount 1))
	    	    ));pif arc

		

	    

		 (IF (AND (/= XLCCHECKER "D")(/= XLCCHECKER "Y"))  (linereader)(setq linetext (nth rocount rolist)
					     rocount (+ rocount 1)))
				));p and while not end of observation				 

;start building lines


(if (> (length normalobs) 0)(progn
  (setq sp (nth 0 normalpoints))
  (setq sobs (nth 0 normalobs))
  (setq normalobs (remove_nth normalobs 0))
  (setq normalobs (remove_nth normalobs 0))
  (setq normalpoints (remove_nth normalpoints 0))
  (setq normalpoints (remove_nth normalpoints 0))
  );if normal
  (progn;if no road try topo
  (if (> (length topoobs) 0)(progn
  (setq sp (nth 0 topopoints))
  (setq sobs (nth 0 topoobs))
  (setq topoobs (remove_nth topoobs 0))
  (setq topoobs (remove_nth topoobs 0))
  (setq topopoints (remove_nth topopoints 0))
  (setq topopoints (remove_nth topopoints 0))
  );if topo
     (progn;if no road try traverse
  (if (> (length travobs) 0)(progn
  (setq sp (nth 0 travpoints))
  (setq sobs (nth 0 travobs))
  (setq travobs (remove_nth travobs 0))
  (setq travobs (remove_nth travobs 0))
  (setq travpoints (remove_nth travpoints 0))
  (setq travpoints (remove_nth travpoints 0))
  );if traverse
    (progn ;if no bdy or road try sideshot
      (if (> (length sideobs) 0)(progn
  (setq sp (nth 0 sidepoints))
  (setq sobs (nth 0 sideobs))
  (setq sideobs (remove_nth sideobs 0))
  (setq sideobs (remove_nth sideobs 0))
  (setq sidepoints (remove_nth sidepoints 0))
  (setq sidepoints (remove_nth sidepoints 0))
  )(exit);if sideshots
	));if no traverse
));if no topo
  ));if no normal
  );if normal


  (if (= XLCCHECKER "Y")(progn;if xlc obs checker find existing coordinate
			  (setq remlist (member sp  cgpl))
			  (setq linetext(nth (- (length cgpl)(length remlist) 3) cgpl))
			  (setq spcpos (vl-string-position 32 linetext ))
                         (setq east (atof (substr linetext ( + spcpos 2) )))
			 (setq north (atof (substr linetext 1  spcpos )))
			(setq startpoint (list east north))
			)
    
    ;(setq startpoint (getpoint "Select a starting point:"));else user suppliers start point
    )
				
  

   (setq cgpointnum (append cgpointnum (list sp)))
  (setq cgpointco (append cgpointco (list startpoint)))
  (setq orderlist (append orderlist (list 1)))
(setq networklist (append networklist (list cgpointnum)))
  (setq nwls sp)
  
(setq obspos 0)
  (SETVAR "CLAYER"  "Boundary" )
  (setq order 1)
 (linebuilder)

 
;(setq obscount (- obscount 1))
  (setq norder (append norder (list target)))
  
(while (/= obscount 0)
  ;(repeat 1000
 (progn

    (setq obsdone "N")

 

      ;if normalpoint found
(if (setq spremlist (member sp normalpoints))
  (progn
    (setq obspos (- (length normalpoints) (length spremlist)))
    (setq sobs (nth obspos normalobs))
    (SETVAR "CLAYER"  "Boundary" )
    (setq order 1)
    (linebuilder)

    (if (= rswitch "T")
      (progn
  (setq normalobs (remove_nth normalobs (- obspos 1)))
  (setq normalobs (remove_nth normalobs (- obspos 1)))
  (setq normalpoints (remove_nth normalpoints  (- obspos 1)))
  (setq normalpoints (remove_nth normalpoints  (- obspos 1)))
  
    )
      (progn
  (setq normalobs (remove_nth normalobs  obspos ))
  (setq normalobs (remove_nth normalobs  obspos ))
  (setq normalpoints (remove_nth normalpoints  obspos ))
  (setq normalpoints (remove_nth normalpoints  obspos ))
	)
      )
    
    (setq obsdone "Y")
    ;(setq obscount (- obscount 1))

       (if (= (member target norder) nil)(setq norder (append norder (list target))))
	
    );p
  );if normalpoints

;if no normal check for topo line from point
(if (and (/= obsdone "Y") (setq spremlist (member sp topopoints)))
  (progn
    (setq obspos (- (length topopoints) (length spremlist)))
    (setq sobs (nth obspos topoobs))
    (SETVAR "CLAYER"  "Topo" )
    (setq order 2)
    (linebuilder)

    (if (= rswitch "T")
      (progn
  (setq topoobs (remove_nth topoobs (- obspos 1)))
  (setq topoobs (remove_nth topoobs (- obspos 1)))
  (setq topopoints (remove_nth topopoints  (- obspos 1 )))
  (setq topopoints (remove_nth topopoints  (- obspos 1 )))
  
    )
      (progn
  (setq topoobs (remove_nth topoobs   obspos ))
  (setq topoobs (remove_nth topoobs   obspos ))
  (setq topopoints (remove_nth topopoints  obspos ))
  (setq topopoints (remove_nth topopoints  obspos ))
	)
      )
    (setq obsdone "Y")
    ;(setq obscount (- obscount 1))
    
    (if (= (member target border) nil)
      (progn
	(setq toorder (append toorder (list target)))
     ; (alert (strcat target " added to border"))
      ))
    )
  );if topopoints

   
;if no normal or topo check for trav line from point
(if (and (/= obsdone "Y") (setq spremlist (member sp travpoints)))
  (progn
    (setq obspos (- (length travpoints) (length spremlist)))
    (setq sobs (nth obspos travobs))
    (SETVAR "CLAYER"  "Traverse" )
    (setq order 2)
    (linebuilder)

    (if (= rswitch "T")
      (progn
  (setq travobs (remove_nth travobs (- obspos 1)))
  (setq travobs (remove_nth travobs (- obspos 1)))
  (setq travpoints (remove_nth travpoints  (- obspos 1 )))
  (setq travpoints (remove_nth travpoints  (- obspos 1 )))
  
    )
      (progn
  (setq travobs (remove_nth travobs   obspos ))
  (setq travobs (remove_nth travobs   obspos ))
  (setq travpoints (remove_nth travpoints  obspos ))
  (setq travpoints (remove_nth travpoints  obspos ))
	)
      )
    (setq obsdone "Y")
    ;(setq obscount (- obscount 1))
    
    (if (= (member target border) nil)
      (progn
	(setq torder (append torder (list target)))
     ; (alert (strcat target " added to border"))
      ))
    )
  );if travpoints


;if no normal or topo or trav check for sideshot line from point
(if (and (/= obsdone "Y") (setq spremlist (member sp sidepoints)))
  (progn
    (setq obspos (- (length sidepoints) (length spremlist)))
    (setq sobs (nth obspos sideobs))
    (SETVAR "CLAYER"  "Sideshot" )
    (setq order 3)
    (linebuilder)

    (if (= rswitch "T")
      (progn
  (setq sideobs (remove_nth sideobs (- obspos 1)))
  (setq sideobs (remove_nth sideobs (- obspos 1)))
  (setq sidepoints (remove_nth sidepoints (- obspos 1 )))
  (setq sidepoints (remove_nth sidepoints (- obspos 1 )))
  
    )
      (progn
  (setq sideobs (remove_nth sideobs   obspos ))
  (setq sideobs (remove_nth sideobs   obspos  ))
  (setq sidepoints (remove_nth sidepoints  obspos ))
  (setq sidepoints (remove_nth sidepoints  obspos ))
	)
      )
    (setq obsdone "Y")
    ;(setq obscount (- obscount 1))
    (if (= (member target sorder) nil)(setq sorder (append sorder (list target))))
    )
  );if bdypoints

;no more obs found from point, go to next point in heirachy
(if (/= obsdone "Y")
  (progn
    (SETVAR "CLAYER"  "Boundary" )
    (setq pointfound "N")
    (if (> (length norder) 0)(setq sp (car norder)
				   norder (remove_nth norder 0)
				   pointfound "Y"))
    (if (and (= (length norder) 0)(> (length toorder) 0)(/= pointfound "Y"))(setq sp (car toorder)
							                         toorder (remove_nth toorder 0)
										 pointfound "Y")
										 )
    (if (and (= (length norder) 0)(= (length toorder) 0)(> (length torder) 0)(/= pointfound "Y"))(setq sp (car torder)
										                      torder (remove_nth torder 0)
												      pointfound "Y"))
        (if (and (= (length norder) 0)(= (length toorder) 0)(= (length torder) 0)(> (length sorder) 0)(/= pointfound "Y"))(setq sp (car sorder)
										                      sorder (remove_nth sorder 0)
												      pointfound "Y"))
    (if (and (= (length norder) 0)(= (length toorder) 0)(= (length torder) 0)(= (length sorder) 0)(/= pointfound "Y"))(setq obscount 0))

    (setq remlist (member sp cgpointnum))
    (setq startpoint (nth (- (length cgpointnum)(length remlist)) cgpointco))
    (setq nwls (nth (- (length cgpointnum)(length remlist)) networklist))
    
    
    ;(alert (strcat "\nPoint Switched to " sp  "-" (rtos obscount 2 0)))
    )
  )

));p and if while obscount

(princ (strcat "\nObservation Import Complete - Largest Misclose:" (rtos largemiss 2 3)))  
   

(if (AND (/= XLCCHECKER "D")(/= XLCCHECKER "Y"))
  (progn
 ;Create GG point list

(close xmlfile)
  (setq xmlfile (open xmlfilen "r"))
  
 
  ;CGPOINTS bdy and control-------------------------------------
  ;linefeed to cgpoints
  (while (= (vl-string-search "<CgPoints" linetext) nil) ( progn
  (linereader)
))


  ;get zone if present
  (if (/= (setq stringpos (vl-string-search "zoneNumber" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 12)))(setq zone (substr linetext (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11)))))(setq zone ""))
  
  (linereader)
  ;do until end of cgpoints
      (while (= (vl-string-search "</CgPoints" linetext) nil) ( progn
  (if (/= (vl-string-search "<CgPoint" linetext )nil)
     (progn
       ;store line information
       (if (/= (setq stringpos (vl-string-search "state" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 7)))(setq cgpstate (substr linetext (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq cgpstate nil))
              (if (/= (setq stringpos (vl-string-search "pntSurv" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))(setq cgpntsurv (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8)))))(setq cgpntsurv nil))
              (if (/= (setq stringpos (vl-string-search "name" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq cgpname (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq cgpname nil))
                     (if (/= (setq stringpos (vl-string-search "oID" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 5)))(setq cgpoID (substr linetext (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4)))))(setq cgpoID nil))
                            (if (/= (setq stringpos (vl-string-search "desc" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq cgdesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq cgdesc nil))


       
       ;REMOVED VIC - NO NEED TO ID PM check if point is a pm and store in drawpmlist
       ;(if (/= cgpoID nil)(progn
;			    (setq remlist (member cgpname pmlist))
;			    (setq pmline (cadr remlist))
;			    (if (/= (setq stringpos (vl-string-search "type" pmline)) nil)(progn
;			    (setq wwpos (vl-string-position 34 pmline (+ stringpos 6)))(setq pmstart (substr pmline (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))
;			      (setq pmstart nil))
;			    (if (/= (setq stringpos (vl-string-search "state" pmline)) nil)(progn
;			    (setq wwpos (vl-string-position 34 pmline (+ stringpos 7)))(setq pmstate (substr pmline (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))
;			      (setq pmstate ""))
;			    (setq drawpmlist (APPEND drawpmlist (list cgpname)(list (strcat pmstart cgpoID ))(list pmstate )))
;			    )
;	 )
              	  
			    
       ;get cgpoint coodintes

       ;IS to cgpname list
(if (setq remlist (member cgpname islist))
       (progn
       (setq cgppos (- (length islist) (length remlist)))
       (setq isname (substr (nth (- cgppos 1)  islist) 4 200))
        
    
       (setq remlist (member isname cgpointnum))


       

	     (if (/= (setq stringpos (vl-string-search ">" linetext )) nil)(progn
(setq <pos (vl-string-position 60 linetext (+ stringpos 2)))))
(if (= <pos nil)(setq <pos 2000))
(setq spcpos (vl-string-position 32 linetext stringpos))
(setq north  (atof (substr linetext (+ stringpos 2) (- spcpos (+ stringpos 1) ))))
(setq east  (atof (substr linetext (+ spcpos 2) (- (- <pos 1) spcpos ))))

       ;if in lats and longs convert to easting northing
(if (vl-string-search "latitude=" linetext )(progn
					      (setq p1 (list east north))
					      (LL2MGA)
					      ))
       
(setq origco (list  east north))
;get original coords for sideshot compare

	   
       (setq cgco (nth (- (length cgpointnum)(length remlist)) cgpointco))
(setq cgcos (strcat (rtos (car cgco)2 6) "," (rtos (cadr cgco) 2 6)))
       (setq cgpointlist (append cgpointlist (list cgpname) (list cgcos)(list (substr cgpntsurv 1 1))))
	   (setq origcgpointlist (append origcgpointlist  (list origco)))
	   (setq newcgpointlist (append newcgpointlist (list cgco)))
           (setq east (car cgco))
	   (setq north (cadr cgco)) 
      	   (if (> east maxeast) (setq maxeast east))
           (if (< north minnorth)(setq minnorth north))
           (if (> north maxnorth)(setq maxnorth north))
	   

	


        ;DRAW CG POINTS
       (if (= simplestop "0")(progn
			       
       (SETVAR "CLAYER"  "CG Points" )
       (setq p1 (list  east north))
       
       (command "point" p1)
       (COMMAND "TEXT" "J" "BL"  P1 (* TH 0.25) "90" (strcat cgpname (substr cgpntsurv 1 1)(substr cgpstate 1 1)))
       ))
)
   );if not sideshot

       ;if datum point draw datum point and label
       
(if (and (/= cgdesc nil)(= simplestop "0"))(progn
		   	 (SETVAR "CLAYER"  "Datum Points" )
		      (COMMAND "POINT" cgco)
		 (SETQ SENT (ENTLAST))
  (SETQ SENTLIST (ENTGET SENT))
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 cgdesc)))))
		     (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
       (SETQ TEXTPOS (LIST (-  (car cgco) TH) (+  (cadr cgco) (* 0.5 TH))))
		 (SETVAR "CLAYER"  "Drafting" )
  		 (COMMAND "TEXT" "J" "BR"  TEXTPOS (* TH 2) "90" cgdesc)
  ));p&if datum
       


       
       ))
(linereader)

   ));p&while not cgpoint end

  

  ;start again to establish shifts for sideshots
       (close xmlfile)
  (setq xmlfile (open xmlfilen "r"))
      
 
  ;CGPOINTS sideshots-------------------------------------
  ;linefeed to cgpoints

   (while (= (vl-string-search "<CgPoints" linetext) nil) ( progn
 (linereader)
))


 (linereader)
  ;do until end of cgpoints
      (while (= (vl-string-search "</CgPoints" linetext) nil) ( progn
   (if (/= (vl-string-search "<CgPoint" linetext )nil)
     (progn
       ;store line information
       (if (/= (setq stringpos (vl-string-search "state" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 7)))(setq cgpstate (substr linetext (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq cgpstate nil))
              (if (/= (setq stringpos (vl-string-search "pntSurv" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 9)))(setq cgpntsurv (substr linetext (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8)))))(setq cgpntsurv nil))
              (if (/= (setq stringpos (vl-string-search "name" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq cgpname (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq cgpname nil))
                     (if (/= (setq stringpos (vl-string-search "oID" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 5)))(setq cgpoID (substr linetext (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4)))))(setq cgpoID nil))
                            (if (/= (setq stringpos (vl-string-search "desc" linetext )) nil)(progn
(setq wwpos (vl-string-position 34 linetext (+ stringpos 6)))(setq cgdesc (substr linetext (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq cgdesc nil))

              	  
			    
       ;get cgpoint coodintes

       ;possibly need an IS to cgpname list here later on
       
       (if (= (member cgpname islist) nil)
	 (progn


	   (if (/= (setq stringpos (vl-string-search ">" linetext )) nil)(progn
(setq <pos (vl-string-position 60 linetext (+ stringpos 2)))))
(if (= <pos nil)(setq <pos 2000))
                          
(setq spcpos (vl-string-position 32 linetext stringpos))
(setq north (atof (substr linetext (+ stringpos 2) (- spcpos (+ stringpos 1) ))))
(setq east (atof (substr linetext (+ spcpos 2) (- (- <pos 1) spcpos ))))

	    ;if in lats and longs convert to easting northing
(if (vl-string-search "latitude=" linetext )(progn
					      (setq p1 (list east north))
					      (LL2MGA)
					      ))
	   
(setq cgss (list east north))


	   ;find closest new position to establish shift for sideshot
(setq mindist 10000000000000000000000000000000000000000000000000000000000000000000)
(setq sscount 0)
(repeat (length origcgpointlist)
  (setq origco (nth sscount origcgpointlist))
  (setq newco (nth sscount newcgpointlist))
  (if (< (distance cgss origco) mindist)(setq shift (list (- (car newco)(car origco))(- (cadr newco)(cadr origco)))
							       mindist (distance cgss origco)))
  (setq sscount (+ sscount 1))
    )

(setq cgcos (strcat (rtos(+ (car shift) east) 2 6) "," (rtos(+ (cadr shift) north)2 6)))

 (setq cgpointlist (append cgpointlist (list cgpname) (list cgcos)(list (substr cgpntsurv 1 1))))

	   ;check for max east min north
	   
	   (if (> (+ (car shift) east) maxeast) (setq maxeast (+ (car shift) east)))
           (if (< (+ (cadr shift) north) minnorth)(setq minnorth (+ (cadr shift) north)))

	   ;DRAW CG POINTS   
       (if (= simplestop "0")(progn
       (SETVAR "CLAYER"  "CG Points" )
       (setq p1 (list  (+ (car shift) east)  (+ (cadr shift) north)))
       (command "point" p1)
       (COMMAND "TEXT" "J" "BL"  P1 (* TH 0.25) "90" (strcat cgpname (substr cgpntsurv 1 1)(substr cgpstate 1 1)))
	   ))
));p&if sideshot

    

       ))
  (linereader)
   ));p&while not cgpoint end

 
));if not xlcchecker
 (if (= simplestop "0")(command "erase" dellines ""))
  )     


 

  

(defun c:xsw (/) ; SWAP TEXT POSITIONS


  (SETQ LINES (SSGET  '((0 . "TEXT"))))

  (SETQ COUNT 0)

(SETQ P1 (CDR(ASSOC 11 (ENTGET (SSNAME LINES COUNT)))))
(SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME LINES (+ COUNT 1))))))
  (SETQ EN1 (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ EN2 (CDR(ASSOC -1 (ENTGET (SSNAME LINES (+ COUNT 1))))))

  (SETQ EN1G (ENTGET EN1))
  (SETQ EN2G (ENTGET EN2))
  
  (SETQ	EN1G (subst (cons 11  P2)(assoc 11 EN1G) EN1G ) )
 
  (ENTMOD EN1G)

  (SETQ	EN2G (subst (cons 11  P1)(assoc 11 EN2G) EN2G ) )
  (ENTMOD EN2G)

  )

(defun c:xsp (/) ;SPIN TEXT 180


  (SETQ LINES (SSGET  '((0 . "TEXT"))))

  (SETQ COUNT 0)

  (repeat (sslength lines)
(SETQ P1 (CDR(ASSOC 11 (ENTGET (SSNAME LINES COUNT)))))
(SETQ ROT (CDR(ASSOC 50 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ EN1 (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))

    (SETQ ROT (+ ROT PI))
    (IF (> ROT (* 2 PI))(SETQ ROT (- ROT (* 2 PI))))

  (SETQ EN1G (ENTGET EN1))
    
  (SETQ	EN1G (subst (cons 50  ROT)(assoc 50 EN1G) EN1G ) )
 
  (ENTMOD EN1G)

    (SETQ COUNT (+ COUNT 1))
    )

  )

(defun C:XSB (/)

  (PRINC "\nSelect bearings to spread:")
  (SETQ PLAYER (GETVAR "CLAYER"))
  (SETQ LINES (SSGET  '((0 . "TEXT"))))

  (SETQ COUNT 0)
  (SETQ P1 (CDR(ASSOC 11 (ENTGET (SSNAME LINES COUNT)))))
(SETQ ROT (CDR(ASSOC 50 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ EN1 (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ TEXTHEIGHT (CDR(ASSOC 40 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ ZA (CDR(ASSOC 210 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ LAYER (CDR(ASSOC 8 (ENTGET (SSNAME LINES COUNT)))))
  (SETVAR "CLAYER" LAYER)
(SETQ TEXT (CDR(ASSOC 1 (ENTGET (SSNAME LINES COUNT)))))
(SETQ TEXTCHK TEXT)
  (SETQ P1 (TRANS P1 ZA 0))
  
  (SETQ MINX (CAR P1))
  (SETQ MAXX (CAR P1))

  (SETQ SP P1)
  (SETQ EP P1)
  
  (SETQ COUNT 1)
  (REPEAT (-(SSLENGTH LINES) 1)

    (SETQ P1 (CDR(ASSOC 11 (ENTGET (SSNAME LINES COUNT)))))
(SETQ ANG (CDR(ASSOC 50 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ EN1 (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))
(SETQ TEXT (CDR(ASSOC 1 (ENTGET (SSNAME LINES COUNT)))))
    (SETQ P1 (TRANS P1 ZA 0))
    

    (IF (/= TEXT TEXTCHK)(PROGN
			   (PRINC "\nAll bearings not the same")
			   (QUIT)
			   ))
    (IF (> (CAR P1) MAXX)(SETQ EP P1
			       MAXX (CAR P1)))
    
    (IF (< (CAR P1) MINX)(SETQ SP P1
			       MINX (CAR P1)))
    
(SETQ COUNT (+ COUNT 1))
    )

  (if (/= (setq wwpos (vl-string-position 34 text 0)) nil)(progn ;seconds exist so 3 spread
						  (setq degpos (vl-string-position 176 text 0))
						  (setq wpos (vl-string-position 39 text 0))
						  (setq deg (substr text 1 (+ degpos 1)))
						  (setq mins (substr text (+ degpos 2)(- wpos degpos)))
						  (setq sec (substr text (+ wpos 2) 50))
						  (setq mp (list (/ (+ (CAR SP)(CAR EP)) 2)(/ (+ (CADR SP)(CADR EP)) 2)))

						  
						  (COMMAND "UCS" "W")
						  (COMMAND "TEXT" "J" "MC"   sp   TEXTHEIGHT (ANGTOS ANG 1 4) DEG)
						  (COMMAND "TEXT" "J" "MC"   mp   TEXTHEIGHT (ANGTOS ANG 1 4) MINS)
						  (COMMAND "TEXT" "J" "MC"   ep   TEXTHEIGHT (ANGTOS ANG 1 4) SEC)
						  (command "erase" lines "")
						  (COMMAND "UCS" "P")

						  )
    (progn ;else deg and mins
      (setq degpos (vl-string-position 176 text 0))
      (setq deg (substr text 1 (+ degpos 1)))
      (setq mins (substr text (+ degpos 1) 50))
     
      (COMMAND "UCS" "W")
       (COMMAND "TEXT" "J" "MC"   sp  TEXTHEIGHT (ANGTOS ANG 1 4) DEG)
       (COMMAND "TEXT" "J" "MC"   ep  TEXTHEIGHT (ANGTOS ANG 1 4) MINS)
      (command "erase" lines "")
      (COMMAND "UCS" "P")
      )
    )
  (SETVAR "CLAYER" PLAYER)
  )
						  
      

;------------------------------------------------create brackets around text ------------------------------
(DEFUN C:XCB (/)
						  
      
  
  (SETQ LINES (SSGET  '((0 . "TEXT"))))

  (SETQ COUNT 0)

  (repeat (sslength lines)
(SETQ P1 (CDR(ASSOC 11 (ENTGET (SSNAME LINES COUNT)))))
(SETQ TEXT (CDR(ASSOC 1 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ EN1 (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))

    (SETQ TEXT (STRCAT "(" TEXT ")"))

  (SETQ EN1G (ENTGET EN1))
    
  (SETQ	EN1G (subst (cons 1 TEXT)(assoc 1 EN1G) EN1G ) )
 
  (ENTMOD EN1G)

    (SETQ COUNT (+ COUNT 1))
    )
 

  )

;----------------------------------------------------relabel text from xdata----------------------------------------------------
(DEFUN C:XRT (/)
(setq curlayer (getvar "CLAYER"))
  (SETQ LINES (SSGET  '((0 . "LINE,POINT,ARC,LWPOLYLINE,POLYLINE"))))

  (SETQ COUNT 0)
(REPEAT (SSLENGTH LINES)
  (SETQ OBJTYPE  (CDR(ASSOC 0 (ENTGET (SSNAME LINES COUNT)))))
(SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ LAYER (CDR(ASSOC 8 (ENTGET (SSNAME LINES COUNT)))))
(setvar "CLAYER" (CDR(ASSOC 8 (ENTGET (SSNAME LINES COUNT)))))


  
  
;LINE WITH BEARING AND DISTANCE
  (IF (AND (= OBJTYPE "LINE") (/= (SUBSTR LAYER 1 10)  "Occupation"))
    (PROGN
  (SETQ P1 (LIST (CAR P1) (CADR P1)));2DISE P1 TO GIVE 2D DISTANCE
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME LINES COUNT)))))
  

    (setq p1 (trans p1 0 1))
  (setq p2 (trans p2 0 1))

  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))

		     (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Selected Line has no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))

   (setq stringpos (vl-string-search "azimuth" xdatai ))
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 9)))
            (setq bearing (substr xdatai (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))))
	    (setq xbearing bearing)

	    (setq stringpos (vl-string-search "horizDistance" xdatai ))
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 15)))
            (setq dist (substr xdatai (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14))))

    (if (/= (setq stringpos (vl-string-search "distanceType=" xdatai ))nil)(progn
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 14)))
            (setq distancetype (strcat " distanceType=\"" (substr xdatai (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13))) "\" ")))(setq distancetype ""))

	    (if (/= (setq stringpos (vl-string-search "azimuthType=" xdatai ))nil)(progn
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 13)))
            (setq azimuthtype (strcat " azimuthType=\"" (substr xdatai (+ stringpos 14) (-(- wwpos 1)(+ stringpos 12))) "\" ")))(setq azimuthtype ""))
	  	    

	    (if (/= (setq stringpos (vl-string-search "<FieldNote>" xdatai )) nil)(progn
											   
(if (setq wwpos (vl-string-position 34 xdatai (+ stringpos 12)))
  (progn;if field not contains ""s
    (setq comment (substr xdatai (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11))))
    )
  (progn; else use the < to get other end of field note
    (setq <pos (vl-string-position 60 xdatai (+ stringpos 11)))
    (setq comment (substr xdatai (+ stringpos 12) (-(- <pos 1)(+ stringpos 10))))
    )
  )
)
  (setq comment ""))

  (if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
  (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins   (substr bearing (+ dotpt1 2) 2) )
  (if (= (strlen mins) 1)(setq mins (strcat  mins "0")));fix problem with truncating zeros on minutes and seconds
  (setq mins (strcat mins "'"))
  (setq sec  (substr bearing (+ dotpt1 4) 10))
  (if (= (strlen sec) 1) (setq sec (strcat sec "0")))

  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))
  
  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

      
(setq bearing (strcat  deg "d" mins sec))
    ;(setq lbearing bearing)
	    (setq dist (rtos (atof dist)2 3));remove trailing zeros
	    
  (setq ldist (strcat dist ))

  (command "line" p1 p2 "")
  (setq delent (entlast))

  
  (lba)

  (command "erase" delent "")

  ))

  ;MONUMENT
  (IF (AND (= OBJTYPE "POINT") (= LAYER "Monument"))
    (PROGN

       (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))

      (setq p1 (trans p1 0 1))
      
		     (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Selected Monument has no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))

      
 (if (/= (setq stringpos (vl-string-search "type" xdatai )) nil)(progn
(setq stringpos (vl-string-search "type" xdatai ))(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq rmtype (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq rmtype ""))
 (if (/= (setq stringpos (vl-string-search "state" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 7)))(setq rmstate (substr xdatai (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq rmstate ""))
(if (/= (setq stringpos (vl-string-search "condition" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 11)))(setq rmcondition (substr xdatai (+ stringpos 12) (-(- wwpos 1)(+ stringpos 10)))))(setq rmcondition ""))
(if (/= (setq stringpos (vl-string-search "desc" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq rmcomment (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq rmcomment ""))
(setq &pos 0)
	     (if (/= (setq stringpos (vl-string-search "originSurvey" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 14)))(setq rmrefdp (substr xdatai (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq rmrefdp ""))

(lcm)
))

;DATUM POINTS
   (IF (AND (= OBJTYPE "POINT") (= LAYER "Datum Points"))
    (PROGN

       (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))
      (setq dppos (TRANS p1 0 1))
      (SETQ height (caddr dppos))

      (SETQ TEXTPOS (LIST (- (CAR dpPOS) TH) (+ (CADR dpPOS) (* 0.5 TH))))
      (setq height (caddr dppos))

		     (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Selected Datum Point has no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
      
(setq ab xdatai)

      
	 (SETVAR "CLAYER"  "Drafting AFR" )
      (if (/= height 0)
  		 (progn;stratum datum point
			 (COMMAND "TEXT" "J" "BR"  TEXTPOS (* TH 1.4) "90"  ab )
			 (COMMAND "TEXT" "J" "BL"  dppos (* TH 1) "45" (rtos  height 2 3))
			 );P
	       (COMMAND "TEXT" "J" "BR"  TEXTPOS (* TH 2) "90"  (STRCAT "'" ab "'") );normal datum point
		       );IF SPCPOS2 NIL
  
  (setvar "clayer" prevlayer)
  
  ))      
      


  ;PM'S
  (IF (AND (= OBJTYPE "POINT") (= LAYER "PM"))
    (PROGN

       (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))
      (setq pmpos (TRANS p1 0 1))
      

      (SETQ TEXTPOS (LIST (- (CAR pmPOS) TH) (+ (CADR pmPOS) (* 0.5 TH))))
      (setq height (caddr pmpos))

		     (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Selected PM has no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))

       (if (/= (setq stringpos (vl-string-search "desc" xdatai )) nil)(progn
(setq stringpos (vl-string-search "desc" xdatai ))(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq PMNUM (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq PMNUM ""))
      
	 (SETVAR "CLAYER"  "Drafting AFR" )
  
 (IF (= (SUBSTR PMNUM 1 3) "PCM")
     (COMMAND "._INSERT" "VPCM" "_S" TH pmpos "0");DRAW PCM BLOCK
     (COMMAND "._INSERT" "VPM" "_S" TH pmpos "0");ELSE PM BLOCK
     )
  
  (SETQ RMB (ENTLAST))
  (SETQ ENTSS (SSADD))
  (SSADD RMB ENTSS)
(COMMAND "DRAWORDER" ENTSS "" "FRONT")

  (IF (= (SUBSTR PMNUM 1 3) "PCM")
    (PROGN ;IF PCM
      (SETQ AANG (/ pi 2))
(SETQ 3POS (POLAR PMPOS (- AANG (* 0.5 PI)) (* TH 3)))
      (COMMAND "TEXT" "J" "BL"  3POS  TH  "45" PMNUM)
      )
    (PROGN ;ELSE PM
      

(SETQ TEXTPOS (LIST (+ (CAR PMPOS) TH) (+ (CADR PMPOS) (* 0.5 TH))))
 
  ;(IF (= rmstate "Placed")  (SETQ PMNUMS (STRCAT PMNUM " PL"))(SETQ PMNUMS PMNUM))
		 (COMMAND "TEXT" "J" "BL"  TEXTPOS (* TH 1.4) "90" PMNUM)
  (SETQ TEXTPOS (LIST (+ (CAR PMPOS) TH) (+ (CADR PMPOS) (* -1.25 TH))))
  ;NSW(IF (and (/= pmclass "U") (= pmsource "SCIMS" ))(COMMAND "TEXT" "J" "BL"  TEXTPOS (* TH 1.4) "90" "(EST)"))
  ))

      
  ))      



  
  

  
;ARC
 (IF (AND (= OBJTYPE "ARC") (/= (SUBSTR LAYER 1 10)  "Occupation"))
    (PROGN

       (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))

      (SETQ CP (CDR(ASSOC 10 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ RADIUS (CDR(ASSOC 40 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ LAYER (CDR(ASSOC 8 (ENTGET (SSNAME LINES COUNT)))))

  (SETQ ANG1 (CDR(ASSOC 50 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ ANG2 (CDR(ASSOC 51 (ENTGET (SSNAME LINES COUNT)))))

  (SETQ P1 (POLAR CP ANG1 RADIUS))
  (SETQ P2 (POLAR CP ANG2 RADIUS))
  (SETQ ANG (ANGLE P1 P2))

		     (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Selected Line has no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))


	  	  
	    (setq stringpos (vl-string-search "chordAzimuth" XDATAI ))
	    (setq wwpos (vl-string-position 34 XDATAI (+ stringpos 14)))
            (setq bearing (substr XDATAI (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13))))
	    (setq xbearing bearing)

	    (setq stringpos (vl-string-search "length" XDATAI ))
	    (setq wwpos (vl-string-position 34 XDATAI (+ stringpos 8)))
            (setq arclength (substr XDATAI (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
	    (setq arclength (rtos (atof arclength)2 3));remove trailing zeros

	    (setq stringpos (vl-string-search "radius" XDATAI ))
	    (setq wwpos (vl-string-position 34 XDATAI (+ stringpos 8)))
            (setq radius (substr XDATAI (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
	    

	    (setq stringpos (vl-string-search "rot" XDATAI ))
	    (setq wwpos (vl-string-position 34 XDATAI (+ stringpos 5)))
            (setq curverot (substr XDATAI (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4))))

      (if (/= (setq stringpos (vl-string-search "distanceType=" xdatai ))nil)(progn
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 14)))
            (setq distancetype (strcat " distanceType=\"" (substr xdatai (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13))) "\" ")))(setq distancetype ""))

	    (if (/= (setq stringpos (vl-string-search "azimuthType=" xdatai ))nil)(progn
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 13)))
            (setq azimuthtype (strcat " azimuthType=\"" (substr xdatai (+ stringpos 14) (-(- wwpos 1)(+ stringpos 12))) "\" ")))(setq azimuthtype ""))

	    
	    (if (/= (setq stringpos (vl-string-search "arcType" XDATAI )) nil)(progn
	    (setq wwpos (vl-string-position 34 XDATAI (+ stringpos 9)))
            (setq arcType (strcat " arcType=\"" (substr XDATAI (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))) "\"")))(setq arcType ""))

	    (if (setq stringpos (vl-string-search "angleAccClass" XDATAI ))(progn
	    (setq wwpos (vl-string-position 34 XDATAI (+ stringpos 15)))
            (setq sdb (substr XDATAI (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq sdb ""))

	    (if (setq stringpos (vl-string-search "distanceAccClass" XDATAI ))(progn
	    (setq wwpos (vl-string-position 34 XDATAI (+ stringpos 18)))
            (setq sdd (substr XDATAI (+ stringpos 19) (-(- wwpos 1)(+ stringpos 17)))))(setq sdd ""))
		  

	       (if (/= (setq stringpos (vl-string-search "<FieldNote>" XDATAI )) nil)(progn
											   
(if (setq wwpos (vl-string-position 34 XDATAI (+ stringpos 12)))
  (progn;if field not contains ""s
    (setq comment (substr XDATAI (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11))))
    )
  (progn; else use the < to get other end of field note
    (setq <pos (vl-string-position 60 XDATAI (+ stringpos 11)))
    (setq comment (substr XDATAI (+ stringpos 12) (-(- <pos 1)(+ stringpos 10))))
    )
  )
)
  (setq comment ""))
	    (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&amp;" comment &pos )) nil) (setq comment (vl-string-subst "&" "&amp;"  comment &pos)
										      &pos (+ &pos 1)))


      (if (= curverot "cw")(setq p1p p1
				 p1 p2
				 p2 p1p))
	


	    (setq digchaz (angle p1 p2))

;calc arc internal angle
	      (SETQ MAST (SQRT (- (* (atof RADIUS) (atof RADIUS)) (* (/ (distance p1 p2) 2)(/ (distance p1 p2) 2 )))))
  (SETQ O (* 2 (ATAN (/ (/ (distance p1 p2) 2) MAST))))
	    (setq remhalfO  (- (* 0.5 pi) (/ O 2)))
	    ;calc bearing from p1 to arc centre (watching for bulbous arcs)
	    (if (and (= curverot "ccw") (<= (atof arclength) (* pi (atof radius))))(setq raybearing (+  digchaz  remhalfO)))
	    (IF (and (= curverot "cw") (<= (atof arclength) (* pi (atof radius))))(setq raybearing (-  digchaz  remhalfO)))
	    (IF (and (= curverot "ccw") (> (atof arclength) (* pi (atof radius))))(setq raybearing (-  digchaz  remhalfO)))
	    (if (and (= curverot "cw") (> (atof arclength) (* pi (atof radius))))(setq raybearing (+  digchaz  remhalfO)))

	    
	      ;CONVERT TO ANTI CLOCKWISE AND EAST ANGLE
  ;(SETQ raybearing (+ (* -1 raybearing) (* 0.5 PI)))

	       ;calc curve centre point
	    (setq curvecen (polar p1 raybearing (atof radius)))
	    (setq curvecenc (strcat (rtos (car curvecen) 2 9) "," (rtos (cadr curvecen) 2 9)))

	    ;calc curve midpoint
  (setq a1 (angle curvecen p1))
  (setq a2 (angle curvecen p2))
  (if (= curverot "ccw")(setq da (- a2 a1))(setq da (- a1 a2)))
  (if (< da 0)(setq da (+ da (* 2 pi))))
    (SETQ DA (/ DA 2))
    (IF (= CURVEROT "ccw")(setq midb (+ a1 da))(setq midb (+ a2 da)))
  (setq amp (polar curvecen midb (atof radius)))
	    
	    
  					   
;calc chord distance, note using string values not digital values
	    (setq stringO (/ (atof arclength) (atof radius)));arc internal angle based on string values
	    (setq dist (rtos (* 2 (atof radius) (sin (/ stringO 2))) 2 3))

       (if (= qround "YES")(progn 
	(SETQ LLEN (atof dist))

  (IF (< LLEN DISTMAX1) (SETQ DROUND DRND1))
    (IF (AND (> LLEN DISTMAX1)(< LLEN DISTMAX2)) (SETQ DROUND DRND2))
    (IF (> LLEN DISTMAX2)(SETQ DROUND DRND3))
			
    (SETQ LIP (FIX (/ LLEN DROUND)))
    (SETQ LFP (- (/ LLEN DROUND) LIP))
    (IF (>= LFP 0.5 ) (SETQ LIP (+ LIP 1)))
    (SETQ LLEN (* LIP DROUND))
    
    (SETQ dist (RTOS LLEN 2 3))
    ;(IF (< LLEN 1) (SETQ DTS (STRCAT "0" DTS)))

	))

	       ;APPLY ALL CORRECTIONS AND EXTRACT INFORMATION FROM USER INPUT
(if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
   (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins   (substr bearing (+ dotpt1 2) 2) )
  (if (= (strlen mins) 1)(setq mins (strcat  mins "0")));fix problem with truncating zeros on minutes and seconds
  (setq mins (strcat mins "'"))
  (setq sec  (substr bearing (+ dotpt1 4) 10))
  (if (= (strlen sec) 1) (setq sec (strcat sec "0")))

 

  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))

  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

      
(setq bearing (strcat  deg "d" mins sec))
    ;(setq lbearing bearing)'
	     (setq dist (rtos (atof dist)2 3));remove trailing zeros
  (setq ldist dist )
	    (setq radius (rtos (atof radius)2 3));remove trailing zeros
    (setq lradius radius)




	        ;if nothing at either end of line
	   ;(if (= (or (member setupid pmlist)(member targetid pmlist)(member targetid rmlist)(member setupid rmlist)) nil)(progn

      (SETVAR "CLAYER" layer )
      
(setq tp1 p1)
(setq tp2 p2)
      
  (lbarc);label line if not already labelled;label arc using function

    ))




  ;---------------------Occupations----------------------

  
    (if (= (SUBSTR LAYER 1 10) "Occupation")
      (PROGN

 (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (/= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL)

	      (progn
	      	    
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
 
	(setq irbd xdatai)

	(if (= OBJTYPE "LWPOLYLINE")
	  (PROGN

			       ;if occupations layer append description in front of chainage list
			       (if (= (cdr (assoc 8 (ENTGET EN))) "Occupations")
				 (progn
				     (SETQ XDATAI (ENTGET EN '("LANDXML")))
	   (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     
	      (princ (strcat "\nChainage object has no chainages"))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
				   (if (/= (substr xdatai 1 2) "00");if description already exists
				     (progn
				     (setq ,pos1 (vl-string-position 44 xdatai 0))
				     (setq xdatai (substr xdatai 1 ,pos1 ))
				     ))

				   (setq irbd xdatai)
				   
				   ));if occupaton
				   
  
  (SETQ PTLIST (LIST))
	    (setq sentlist (entget en))
	    
	    (foreach a sentlist
	      (if (= 10 (car a))

		(setq PTLIST (append PTLIST (list (cdr a))))
	      )				;IF
	      
	    )				;FOREACH 			
)
    );IF LWPOLYLINE

  (if (= OBJTYPE "POLYLINE")(PROGN

  (setq enlist (entget en))
	    (setq ptList (list));EMPTY LIST
	    (setq en2 (entnext en))
	    (setq enlist2 (entget en2))
               
	     (while
	      (not
		(equal (cdr (assoc 0 (entget (entnext en2)))) "SEQEND")
	      )
	      	(if (= (cdr(assoc 70 enlist2)) 16)(progn
	       	 (setq ptList (append ptList (list (cdr (assoc 10 enlist2)))))
		))
		 	       
	       (setq en2 (entnext en2))
	       (setq enlist2 (entget en2))
	       );W
   (setq ptList
			(append ptList (list (cdr (assoc 10 enlist2))))
		 
	       )
  ));IF 2D POLYLINE

   (if (= OBJTYPE "LINE")(PROGN

  (setq PTLIST (LIST (cdr (assoc 10 (ENTGET EN)))))
  (SETQ PTLIST (append ptList (list (cdr (assoc 11 (ENTGET EN))))))
		 
	       
  ));IF LINE

  (if (= OBJTYPE "ARC")(PROGN
			 
(SETQ CP (CDR (assoc 10 (ENTGET EN))))
  (SETQ RADIUS (CDR (assoc 40 (ENTGET EN))))
  (SETQ ANG1 (CDR (assoc 50 (ENTGET EN))))
  (SETQ ANG2 (CDR (assoc 51 (ENTGET EN))))
  (SETQ P1 (POLAR CP ANG1 RADIUS))
  (SETQ P2 (POLAR CP ANG2 RADIUS))
(SETQ PTLIST (LIST P1))
(SETQ PTLIST (APPEND PTLIST (LIST P2)))
));IF ARC
  
	(if (and (= objtype "LINE")(= layer "Occupations"))
        (progn;if occ line

	  
	   (if (/= (setq stringpos (vl-string-search "desc" xdatai )) nil)(progn
(setq stringpos (vl-string-search "desc" xdatai ))(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq offs (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq offs ""))

	  (setq occpnt (trans (cdr (assoc 10 (ENTGET EN))) 0 1))
	  (setq p5  (trans (cdr (assoc 11 (ENTGET EN))) 0 1))
	  (setq off (distance occpnt p5))
	  (setq mp (list (/ (+ (car occpnt)(car p5)) 2)(/ (+ (cadr occpnt)(cadr p5)) 2)))
	  
	  (setq ang (angle occpnt p5 ))
	  
	  (if (and (> ang  (* 0.5 pi))(< ang (* 1.5 pi)))(setq ang (- ang pi)))
  (if (< ang 0)(setq ang (+ ang (* 2 pi))))

  

	  (if (> off (* th 5))(setq tpos mp
			    just "BC"))

					  (if (and (< off (* th 7))(>= (angle occpnt p5) (* 0.5 pi))(<= (angle occpnt p5)(* 1.5 pi)))(setq tpos p5
																	 just "BR"))
					  (if (and (< off (* th 7))(or(<= (angle occpnt p5) (* 0.5 pi))(>= (angle occpnt p5)(* 1.5 pi))))(setq tpos p5
																	 just "BL"))
	(setvar "clayer" "Drafting AFR")
	(COMMAND "TEXT" "J" JUST TPOS TH (ANGTOS ANG 1 4) offs)


	  )


	  (progn;anything but an occ line
  
		 (setq mp1 (TRANS (nth (- (/ (length ptlist) 2)1) ptlist) 0 1))
		 (setq mp2 (TRANS (nth  (/ (length ptlist) 2) ptlist) 0 1))
		 (setq mp (list (/ (+ (car mp1)(car mP2)) 2) (/ (+ (cadr mp1)(cadr mp2)) 2)))
		 (setq mprot (angle mp1 mp2))
		 (setq mprot90 (+ mprot (* 0.5 pi)))
  		 (SETQ 1POS (POLAR mp mprot90 (* TH 2.5)))
                 (IF (AND (> mprot  (* 0.5 pi)) (< mprot (* 1.5 pi)))(setq mprot (+ mprot pi))) ;if text is upsidedown reverse rotation
                 (setvar "clayer" "Drafting AFR")
  		 (COMMAND "TEXT" "J" "MC" 1pos TH (ANGTOS mprot 1 4) irbd)
		 (setvar "clayer" prevlayer)
	    );if not in occupations layer

	  )

	    )
	    (progn
	     
	      (princ (strcat "\nOccupation has no xdata" (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))

	));P&IF START OF LAYER IS OCCUPATION
  





  
  
(setq count (+ count 1))
  );r
  (SETVAR "CLAYER"  curlayer )
  );defun

;----------------------------------------------------push text and relabel----------------------------------------------------
(DEFUN C:XPU (/)
 (setq curlayer (getvar "CLAYER"))
  (SETQ LINES (SSGET  '((0 . "LINE"))))

  (SETQ COUNT 0)
(REPEAT (SSLENGTH LINES)
(SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ P1 (LIST (CAR P1) (CADR P1)));2DISE P1 TO GIVE 2D DISTANCE
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ prevlayer (CDR(ASSOC 8 (ENTGET (SSNAME LINES COUNT)))))

  (setq p1 (trans p1 0 1))
  (setq p2 (trans p2 0 1))

  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ ANG (ANGLE P1 P2))
  (SETQ P3 (POLAR P2 ANG (* TH 10)))
  

		     (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR Selected Line has no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))

   (setq stringpos (vl-string-search "azimuth" xdatai ))
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 9)))
            (setq bearing (substr xdatai (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))))
	    (setq xbearing bearing)

	    (setq stringpos (vl-string-search "horizDistance" xdatai ))
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 15)))
            (setq dist (substr xdatai (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14))))

  (if (/= (setq stringpos (vl-string-search "distanceType=" xdatai ))nil)(progn
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 14)))
            (setq distancetype (strcat " distanceType=\"" (substr xdatai (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13))) "\" ")))(setq distancetype ""))

	    (if (/= (setq stringpos (vl-string-search "azimuthType=" xdatai ))nil)(progn
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 13)))
            (setq azimuthtype (strcat " azimuthType=\"" (substr xdatai (+ stringpos 14) (-(- wwpos 1)(+ stringpos 12))) "\" ")))(setq azimuthtype ""))

	  	    

	    (if (/= (setq stringpos (vl-string-search "<FieldNote>" xdatai )) nil)(progn
											   
(if (setq wwpos (vl-string-position 34 xdatai (+ stringpos 12)))
  (progn;if field not contains ""s
    (setq comment (substr xdatai (+ stringpos 13) (-(- wwpos 1)(+ stringpos 11))))
    )
  (progn; else use the < to get other end of field note
    (setq <pos (vl-string-position 60 xdatai (+ stringpos 11)))
    (setq comment (substr xdatai (+ stringpos 12) (-(- <pos 1)(+ stringpos 10))))
    )
  )
)
  (setq comment ""))

  (if (/= (vl-string-position 46 bearing 0) nil ) (PROGN
  (setq dotpt1 (vl-string-position 46 bearing 0))
  (setq deg  (substr bearing 1  dotpt1 ))
  (SETQ mins   (substr bearing (+ dotpt1 2) 2) )
  (if (= (strlen mins) 1)(setq mins (strcat  mins "0")));fix problem with truncating zeros on minutes and seconds
  (setq mins (strcat mins "'"))
  (setq sec  (substr bearing (+ dotpt1 4) 10))
  (if (= (strlen sec) 1) (setq sec (strcat sec "0")))

  
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))
  
  );P
	(progn
	  (setq deg bearing)
	  (setq mins "")
	  (setq sec "")
	  );p else
  
  );IF

      
(setq bearing (strcat  deg "d" mins sec))
    ;(setq lbearing bearing)
	    (setq dist (rtos (atof dist)2 3));remove trailing zeros
	    
  (setq ldist (strcat dist ))

  (setvar "clayer" "Drafting AFR")
  (setvar "celtype" "Easement")
  (command "line" p2 p3 "")
  (setvar "clayer" prevlayer)
  (lba)

  
  
(setq count (+ count 1))
  );r
  (setvar "celtype" "ByLayer")
  (SETVAR "CLAYER"  curlayer )
  );defun  
  
  
						  

;----------------------------------------------------export mountuments to csv file------------------------------
  (Defun c:xmo (/)
   (setq prevlayer (getvar "CLAYER"))
  (SETVAR "CLAYER"  "CG Points" )
      
   (setq fn (getfiled "Output File" "" "TXT" 1)) 
 
  
  (SETQ outfile (OPEN fn "w"))



      (IF (/= (setq bdyline (ssget '((0 . "POINT") (8 . "Monument*")))) nil)(progn

									      (setq ptnum (getreal "Start at:"))
  (setq count 0)
  (repeat (sslength bdyline)


(SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
(SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ ZA (CDR (ASSOC 210 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ P1 (TRANS P1 ZA 0))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR RM point with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
       (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

     (if (/= (setq stringpos (vl-string-search "state" xdatai )) nil)(progn
	(setq wwpos (vl-string-position 34 xdatai (+ stringpos 7)))(setq rmstate (substr xdatai (+ stringpos 8) (-(- wwpos 1)(+ stringpos 6)))))(setq rmstate ""))


    (if (/= (setq stringpos (vl-string-search "type" xdatai )) nil)(progn
	(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq rmtype (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq rmtype ""))

    (if (/= (setq stringpos (vl-string-search "desc" xdatai )) nil)(progn
	(setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))(setq rmdesc (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5)))))(setq rmdesc ""))

    (WRITE-LINE (strcat (rtos  ptnum  2 0) "," (rtos (car p1) 2 3) "," (rtos (cadr p1) 2 3) "," (rtos (caddr p1) 2 3) "," rmtype "-" rmdesc) outfile)

    (command "text" p1 "0.5" "90" (rtos PTNUM 2 0))

    (setq ptnum (+ ptnum 1))
       
  
    (setq count (+ count 1))
    

    );r
  ));p&if monuments

     (SETVAR "CLAYER"  prevlayer )
(CLOSE outfile)
)



;-------------------------------------------------------------AUDIT XML-----------------------------------


(DEFUN C:XAUD (/)

  (setq prevlayer (getvar "CLAYER"))

 (SETQ LINES (SSGET  '((0 . "LINE"))))

  (SETQ COUNT 0)
(REPEAT (SSLENGTH LINES)
(SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ P1 (LIST (CAR P1) (CADR P1)));2DISE P1 TO GIVE 2D DISTANCE
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LINES COUNT)))))
  (SETQ LAYER (CDR(ASSOC 8 (ENTGET (SSNAME LINES COUNT)))))

  (SETQ MPE (/ (+ (CAR P1 ) (CAR P2)) 2))
    (SETQ MPN (/ (+ (CADR P1 ) (CADR P2)) 2))
    (SETQ MP (LIST MPE MPN))


;GET XDATA BEARING AND DISTANCE
  
    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	   (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nBoundary Line with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))


   
    	    (setq stringpos (vl-string-search "azimuth" xdatai ))
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 9)))
            (setq xbearing (substr xdatai (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))))
            (setq stringpos (vl-string-search "horizDistance" xdatai ))
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 15)))
            (setq xdist (atof (substr xdatai (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))


       ;APPLY ALL CORRECTIONS AND EXTRACT INFORMATION FROM USER INPUT
(if (/= (vl-string-position 46 Xbearing 0) nil ) (PROGN
  (setq dotpt1 (vl-string-position 46 Xbearing 0))
  (setq deg  (substr Xbearing 1  dotpt1 ))
  (SETQ mins   (substr Xbearing (+ dotpt1 2) 2) )
  (if (= (strlen mins) 1)(setq mins (strcat  mins "0")));fix problem with truncating zeros on minutes and seconds
  (setq mins (strcat mins "'"))
  (setq sec  (substr Xbearing (+ dotpt1 4) 10))
  (if (= (strlen sec) 1) (setq sec (strcat sec "0")))
  (if (> (strlen sec) 2) (setq sec (strcat (substr sec 1 2) "." (substr sec 3 10))))
  (if (= (strlen sec) 0) (setq sec "") (setq sec (strcat sec (chr 34))))

  )
  (progn
    (setq deg xbearing)
  (setq mins "00")
  (setq sec "00")
  )
  )

  (setq deg (atof deg))
  (setq mins (/ (atof mins) 60) )
  (setq sec (/ (atof sec) 3600))
  (setq xdd (+ deg mins sec))
  

  
  


;GET REAL BEARING 

  (SETQ ANG (ANGLE  P1  P2 ))

  (SETQ LBEARING (ANGTOS ANG 1 4));REQUIRED FOR ELSE ROUND
  (setq bearing (vl-string-subst "d" (chr 176) bearing));added for BricsCAD changes degrees to "d"
  (SETQ LDIST (DISTANCE (LIST (CAR P1)(CADR P1)) P2));REQUIRED FOR ELSE ROUND


   (SETQ SANG (ANGTOS ANG 1 4))
    (setq sang (vl-string-subst "d" (chr 176) sang));added for BricsCAD changes degrees to "d"
    (setq CHRDPOS (vl-string-position 100 SANG 0))
    (setq MINPOS (vl-string-position 39 SANG 0))
    (setq SECPOS (vl-string-position 34 SANG 0))

 ;PARSE ANGLE
    (setq DEG  (substr SANG 1  CHRDPOS ))
    (setq MINS   (substr SANG (+ CHRDPOS 2)  (-(- MINPOS CHRDPOS)1)))
    (setq SEC  (substr SANG (+ MINPOS 2)  (-(- SECPOS MINPOS )1)))
  

   (setq deg (atof deg))
  (setq mins (/ (atof mins) 60) )
  (setq sec (/ (atof sec) 3600))
  (setq ldd (+ deg mins sec))

  (setq ddd1 (- xdd ldd))
  (setq ddd2 (- ldd xdd))
  (if (< ddd1 0) (setq ddd1 (+ ddd1 360)))
  (if (< ddd2 0) (setq ddd2 (+ ddd2 360)))
  (setq ddd (min ddd1 ddd2))
  
  (if (or (> ddd 1) (> (ABS (- XDIST LDIST)) 0.1))
    (PROGN
      
      (COMMAND "CHANGE" en "" "P" "C" "6" "")
      (command "layer" "m" "Miscloses" "c" "Red" "Miscloses" "" )
      (command "text" MP "0.5" "90" (STRCAT "BRG:" (RTOS ddd 2 1) " DST:" (RTOS (- XDIST LDIST) 2 3)))
));if out of tolerance

  

  (SETQ COUNT (+ COUNT 1))

  );R


      (SETVAR "CLAYER" prevlayer)

  );DEFUN
  
  


;----------------------------------------------------COORDINATE TIDY--------------------------------------------


(defun C:XCT (/)

  (setq coords7 nil);list of coords to 7 decimal places
  (setq coords4 nil);list of coords to 4 decimal places

  ;get coordinates of all boundary,road and easement lines and make 2 lists

 ;get  lines
    (princ "\nProcessing Boundary Lines ")
 
(IF (/= (setq bdyline (ssget "_X" '((0 . "LINE") (8 . "Boundary,Boundary Extinguished,Easement")))) nil)(progn 
  (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
	  

    (setq p1s7 (strcat (rtos (car p1) 2 7) "," (rtos (cadr p1) 2 7)))
    (setq p2s7 (strcat (rtos (car p2) 2 7) "," (rtos (cadr p2) 2 7)))
    (setq p1s4 (strcat (rtos (car p1) 2 4) "," (rtos (cadr p1) 2 4)))
    (setq p2s4 (strcat (rtos (car p2) 2 4) "," (rtos (cadr p2) 2 4)))

    (setq coords7 (append coords7 (list p1s7)(list p2s7)))
    (setq coords4 (append coords4 (list p1s4)(list p2s4)))
    

   
    (setq count (+ count 1))
    );r
  );p
 
  );if

;get arcs
  (princ "\nProcessing Boundary Arcs ")
(IF (/= (setq bdyline (ssget "_X" '((0 . "ARC") (8 . "Boundary,Boundary Extinguished,Easement")))) nil)(progn 
 

    (setq count 0)
  (repeat (sslength bdyline)


(SETQ CP (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ RADIUS (CDR(ASSOC 40 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG1 (CDR(ASSOC 50 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG2 (CDR(ASSOC 51 (ENTGET (SSNAME bdyline COUNT)))))

  (SETQ P1 (POLAR CP ANG1 RADIUS))
  (SETQ P2 (POLAR CP ANG2 RADIUS))
;CONVERT TO WCS
    (SETQ ZA (CDR(ASSOC 210 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ CP (TRANS CP ZA 0))
    (SETQ P1 (TRANS P1 ZA 0))
    (SETQ P2 (TRANS P2 ZA 0))
    (if (< (caddr za) 0)(setq TEMP1 P1
			      P1 P2
			      P2 TEMP1))
    

    (setq p1s7 (strcat (rtos (car p1) 2 7) "," (rtos (cadr p1) 2 7)))
    (setq p2s7 (strcat (rtos (car p2) 2 7) "," (rtos (cadr p2) 2 7)))
    (setq p1s4 (strcat (rtos (car p1) 2 4) "," (rtos (cadr p1) 2 4)))
    (setq p2s4 (strcat (rtos (car p2) 2 4) "," (rtos (cadr p2) 2 4)))

    (setq coords7 (append coords7 (list p1s7)(list p2s7)))
    (setq coords4 (append coords4 (list p1s4)(list p2s4)))
    ;(setq cps (strcat (rtos (cadr cp) 2 6) " " (rtos (car cp) 2 6)))

 (setq count (+ count 1))
    );r
  );p
 
  );if

;get lot definitons

(IF (/= (setq lots (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Lot Definitions")))) nil)
  (progn
(setq count 0)
    (repeat (sslength lots);do for number of polylines
      (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME LOTS COUNT)))))

      (setq ed (entget en))
(setq i 0)
      (repeat (length ed)

	(if (= (car (nth i ed)) 10) ;if item is a vertex 
	  (progn
	    (setq vt (cdr (nth i ed))) ; get vertex values

	    (setq testpoint4 (strcat (rtos (car vt) 2 4) "," (rtos (cadr vt) 2 4)))
	    (setq testpoint7 (strcat (rtos (car vt) 2 7) "," (rtos (cadr vt) 2 7)))

	    (if (setq remlist (member testpoint4 coords4))(progn
							    
	      (setq finecoord (nth (- (length coords4) (length remlist)) coords7));check in low precision list and return high precision value

	    (if (/= testpoint7 finecoord)(progn
					     
					     (setq ,pos1 (vl-string-position 44 finecoord 0))
					     (setq east  (substr finecoord 1 ,pos1))
					     (setq north  (substr finecoord (+ ,pos1 2) 50))
					     (setq newpt (list (atof east)(atof north)))
					     (setq ed (subst (cons 10 newpt) (nth i ed) ed))
					     (entmod ed) ; update the drawing
					   (princ (strcat "\nLot Vertex at " (strcat (rtos (car vt) 2 3) "," (rtos (cadr vt) 2 3)) " moved by " (rtos (distance vt newpt) 2 9)))
					     ));p&if not same to 7 dec plcs
	      ));p&if memebr of coords4
	      ));p&if vertex
	(setq i (+ i 1))
	);r polyline
      (setq count (+ count 1))
      );r number of polylines
));p&if lot polylines in drawing

  )
					     

       


										     


;----------------------------------------------------export mountuments to csv file------------------------------
  (Defun c:xmo2 (/)
   (setq prevlayer (getvar "CLAYER"))
  (SETVAR "CLAYER"  "CG Points" )
      
   (setq fn (getfiled "Output File" "" "TXT" 1)) 
 
  
  (SETQ outfile (OPEN fn "w"))
(SETQ aws (SSGET '((0 . "TEXT"))))


      (IF (/= (setq bdyline (ssget "_X" '((0 . "POINT") (8 . "Monument")))) nil)(progn 
  (setq count 0)
  (repeat (sslength bdyline)


(SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
(SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ ZA (CDR (ASSOC 210 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ P1 (TRANS P1 ZA 0))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nERROR RM point with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
       (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))
 (if (/= (setq stringpos (vl-string-search "originSurvey" xdatai )) nil)(progn
	(setq wwpos (vl-string-position 34 xdatai (+ stringpos 14)))(setq OS (substr xdatai (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13)))))(setq OS ""))
   

    (WRITE-LINE OS OUTFILE)

    (command "text" p1 "0.5" "90" (rtos (+ count 1) 2 0))
       
  
    (setq count (+ count 1))
    

    );r
  ));p&if monuments

     (SETVAR "CLAYER"  prevlayer )
(CLOSE outfile)
)



(DEFUN C:XOS (/)

          (setq dist (getstring T (strcat "\nDistance[Meters/Feet/DecimalFeet/Links]" units ":")))

  (if (or (= dist "f") (= dist "F") (= dist "Feet") (= dist "feet") (= dist "FEET"))
    (progn
      (setq dist (getstring T "\nDistance(Feet FF.II.n/d ): "))
      (setq units "F")
      )
    )
  
   (if (or (= dist "d")(= dist "df")(= dist "DF") (= dist "D") (= dist "DecimalFeet") (= dist "decimalfeet") (= DIST "DECIMALFEET"))
    (progn
      (setq dist (getstring T "\nDistance(Decimal Feet): "))
      (setq units "DF")
      )
    )

  (if (or (= dist "l") (= dist "L") (= dist "Links") (= dist "LINKS") (= DIST "links"))
    (progn
      (setq dist (getstring T "\nDistance(Links): "))
      (setq units "L")
      )
    )

    (if (or (= dist "m") (= dist "M") (= dist "Meters") (= dist "meters") (= DIST "METERS"))
    (progn
      (setq dist (getstring T "\nDistance(Meters): "))
      (setq units "M")
      )
    )
  (setq prevdist dist)
 
  
    (if (= units "F")
      (progn
	 (setq dotpos1 (vl-string-position 46 dist 0)) 
		    
	(if (= dotpos1 nil)(setq dotpos2 nil)(setq dotpos2 (vl-string-position 46 dist (+ dotpos1 1))))
	(setq /pos1 (vl-string-position 47 dist 0))
	(if (/= /pos1 nil)
	  (progn
	    (setq den (substr dist ( + /pos1 2) 50))
	    (setq num (substr dist ( + dotpos2 2) (- (- /pos1 dotpos2) 1)))
	    (setq idist (/ (atof num) (atof den)))
	    (setq inches (substr dist (+ dotpos1 2) (- (- dotpos2 dotpos1) 1)))
	    (setq idist (+ idist (atof inches)))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (/= dotpos1 nil) (= /pos1 nil))
	  (progn
	    (setq inches (substr dist ( + dotpos1 2) 50))
	    (setq feet (substr dist 1  dotpos1 ))
	    (setq idist  (atof inches))
	    (setq idist (+ idist (* (atof feet) 12)))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
	(if (and (= dotpos1 nil) (= /pos1 nil) (= dotpos2 nil))
	  (progn
	   
	    (setq feet (substr dist 1  50))
	    (setq idist (* (atof feet) 12))
	    (setq dist (rtos (* idist 0.0254) 2 9))
	    )
	  )
      )
    )
  (if (= units "L")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.201168)))
      )
    )

  (if (= units "DF")
    (progn
      (setq dist (atof dist))
      (setq dist (rtos (* dist 0.3048)))
      )
    )

  (COMMAND "OFFSET" DIST)


  )


;-------------------------------------------------------------------Add note to lines-------------


(defun C:XAN (/)
  

(PRINC "\n Select Lines to Apply Note to:")
  
  
  (setq bdyline (ssget  '((0 . "LINE,ARC") (8 . "Boundary,Traverse,Sideshot,Topo,Easement"))))
  (setq comment (getstring "\nNote:" t))
  (setq count 0)
  (repeat (sslength bdyline)
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
  (setq objType  (CDR(ASSOC 0 (ENTGET (SSNAME bdyline COUNT)))))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nObject has not xdata " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))


;search for normal
    (if (/= (setq stringpos (vl-string-search "/>" xdatai )) nil)
      (progn
	(setq front (substr xdatai 1 stringpos))
	);p
      (progn
	;else fieldontes already exist
	(setq stringpos (vl-string-search ">" xdatai ))
	(setq front (substr xdatai 1 stringpos))
	)
  )

	(if (= objtype "ARC")(setq redob "Arc")(setq redob ""))
  
    

    (setq xdatai (strcat front "><FieldNote>\"" comment  "\"</FieldNote></Reduced" redob "Observation>"))

    (SETQ SENTLIST (ENTGET EN))
  (SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 xdatai)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)

    (setq count (+ count 1))
    )
  );defun

  (defun c:xcd (/)

    (setq cdalf "YXWVUTSRQPNMLKJHGFEDCBA")
(setq PSpos nil
      TPpos nil
      PCpos nil
      BPpos nil)

   
    (setq text (ssget  '((0 . "TEXT,MTEXT"))))
    

    (setq count 0)
    (repeat (sslength text)

       (SETQ ttype (CDR(ASSOC 0 (ENTGET (SSNAME text COUNT)))))
      (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME text COUNT)))))

      
           
      (SETQ textstring (CDR(ASSOC 1 (ENTGET (SSNAME text COUNT)))))
     


      (if (= ttype "MTEXT")
	(PROGN

      (setq mel (entget en))
 (setq textstring "")
 (while (setq str (cdr (assoc 3 mel)))
   (progn
 (setq textstring (strcat textstring str))
 (setq mel (cdr (member (assoc 3 mel) mel)))
 ))
 (setq textstring (strcat textstring (cdr (assoc 1 (entget en)))))

      )
	
	(SETQ textstring (CDR(ASSOC 1 (ENTGET (SSNAME text COUNT)))))
	)

        (setq REFpos 0)

      (setq PSpos (vl-string-search "PS" textstring REFpos ))
      (setq TPpos (vl-string-search "TP" textstring REFpos ))
      (setq PCpos (vl-string-search "PC" textstring REFpos ))
      (setq BPpos (vl-string-search "BP" textstring REFpos ))
      (setq LPpos (vl-string-search "LP" textstring REFpos ))
      (setq CDpos (vl-string-search "CD" textstring REFpos ))
      (setq CPpos (vl-string-search "CP" textstring REFpos ))
      (setq CSpos (vl-string-search "CS" textstring REFpos ))
      (setq APpos (vl-string-search "AP" textstring REFpos ))
      (setq SPpos (vl-string-search "SP" textstring REFpos ))

      ;Note SP not added becuase of confusion with road plans but first SP with chkdgit is SP020337
      ;AP's appear to start getting check digits around AP60000 but it gets confusing with section number
      
	   (while (OR (/= PSpos nil)
		      (/= TPpos nil)
		      (/= PCpos nil)
		      (/= BPpos nil)
		      (/= LPpos nil)
		      (/= CDpos nil)
		      (/= CPpos nil)
		      (/= CSpos nil)
		      (/= APpos nil)
		      (/= SPpos nil))		    
	     (progn
	       ;find minimum number in string
	       (IF (= PSpos nil)(setq PSpos 1000000000000))
	       (IF (= TPpos nil)(setq TPpos 1000000000000))
	       (IF (= PCpos nil)(setq PCpos 1000000000000))
	       (IF (= BPpos nil)(setq BPpos 1000000000000))
	       (IF (= LPpos nil)(setq LPpos 1000000000000))
	       (IF (= CDpos nil)(setq CDpos 1000000000000))
	       (IF (= CPpos nil)(setq CPpos 1000000000000))
	       (IF (= CSpos nil)(setq CSpos 1000000000000))
	       (IF (= APpos nil)(setq APpos 1000000000000))
	       (IF (= SPpos nil)(setq SPpos 1000000000000))
	       
	       (setq refpos (fix(min PSpos TPpos PCpos BPpos LPpos CDpos CPpos CSpos APpos SPpos)))
	       
	       (setq PSpos nil
		     TPpos nil
		     PCpos nil
		     BPpos nil
		     LPpos nil
		     CDpos nil
		     CPpos nil
		     CSpos nil
		     APpos nil
		     SPpos nil)
	       
	       (setq prefix (substr textstring (+ 1 refpos) 2))
	       (setq ref "")
	       
	       (setq 3rdnum (substr textstring (+ 3 refpos) 1))

	       (setq subpos (+ refpos 3))
	       (setq refpos (+ refpos 3))
	       
	       (if (and (>= (ascii 3rdnum) 48)(<= (ascii 3rdnum) 57))
		 (progn
		  	       
	       
	       (while (and
			(>= (ascii (substr textstring subpos 1)) 48)
			(<= (ascii (substr textstring subpos 1)) 57)
			(/= subpos   (+ (strlen textstring)1  ))
			)
		 (progn
		 (setq ref (strcat ref (substr textstring subpos 1)))
		 (setq subpos (+ subpos 1))
		 ));p&w next charater is number

	       (while (< (strlen ref ) 6)(setq ref (strcat "0" ref)))

	       (if (or (= prefix "PS")
		       (= prefix "TP")
		       (= prefix "PC")
		       (and  (= prefix "BP")(> (atof ref) 1699))
		       (= prefix "CD")
		       (and (= prefix "LP")(> (atof ref) 145855))
		       (and (= prefix "CP")(> (atof ref) 157285))
		       (and (= prefix "CS")(> (atof ref) 1386))
		       (and (= prefix "AP")(> (atof ref) 59999))
		       (and (= prefix "SP")(> (atof ref) 20335))

		       )
		 (progn
	       
	        (setq rn (/(+
				  (* (- (ascii (substr prefix 1 1)) 64) 9)
				  (* (- (ascii (substr prefix 2 1)) 64) 8)
				  (* (atof (substr ref 1 1)) 7)
				  (* (atof (substr ref 2 1)) 6)
				  (* (atof (substr ref 3 1)) 5)
				  (* (atof (substr ref 4 1)) 4)
				  (* (atof (substr ref 5 1)) 3)
				  (* (atof (substr ref 6 1)) 2)
				  )
				  23))
		      (setq rn (+ (* (- rn (fix rn)) 23) 1))
	       (setq rn (atof (rtos rn 2 0)))
		       (setq cchkdigit (substr cdalf (fix rn) 1))

	       


	       
	       (if (= subpos (+ (strlen textstring) 1));if no check digit at end of textstring
		 (progn;is at end of textstring
		   (princ (strcat "\n" prefix Ref " missing check digit - check digit should be " cchkdigit))
		   )
		 (progn;if normal check digit

		   (setq chkdigit (substr textstring subpos 1))
		   
		   (if (or (< (ascii chkdigit) 63)
			   (> (ascii chkdigit) 90))
		     (progn;if not a letter of the alphabet
		       (if (= prefix "SP" )(setq endtext " (This may be an RC reference)")(setq endtext ""))
		       (princ (strcat "\n" prefix Ref " missing check digit - check digit should be " cchkdigit endtext))
		       )
		     (progn;if it is a letter of the alphbet
		      
		       (if (/= chkdigit cchkdigit)(princ (strcat "\n" prefix ref chkdigit " is incorrect - check digit calulates as " cchkdigit)))
			      
	       ));if chkdigit is letter of alphabet
	     

		     
	       (princ (strcat "\n" prefix ref chkdigit))

		 ));if check digit is at end of text string
		      ));is a number to check

	       ));if 3rdnum is a number
	       
;check for next number

		 (if (= (>= refpos (strlen textstring)) nil)
		   (progn
	       
      (setq PSpos (vl-string-search "PS" textstring REFpos ))
      (setq TPpos (vl-string-search "TP" textstring REFpos ))
      (setq PCpos (vl-string-search "PC" textstring REFpos ))
      (setq BPpos (vl-string-search "BP" textstring REFpos ))
      (setq LPpos (vl-string-search "LP" textstring REFpos ))
      (setq CDpos (vl-string-search "CD" textstring REFpos ))
      (setq CPpos (vl-string-search "CP" textstring REFpos ))
      (setq CSpos (vl-string-search "CS" textstring REFpos ))
      (setq APpos (vl-string-search "AP" textstring REFpos ))
      (setq SPpos (vl-string-search "SP" textstring REFpos ))


      ));p&if not end of line
	       
	       
	       ));w found
(setq count (+ count 1))
      );r
    );defun
		       
	
      
;----------------------------------------------------------------BULK EDIT replace--------------------------------

(defun C:XBER (/)

  ;0. Datum Points
    (princ "\nSelect Data for Bulk Edit")
 (setq bdyline (ssget))

(setq replace (getstring "\nReplace:" T))
  (setq with (getstring "\nWith:" T))
  
  (setq count 0)
  (repeat (sslength bdyline)



  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
 

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	    (IF (/= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	 
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))

	    
 
(setq xdatai (vl-string-subst with replace xdatai ))
       
 

  (SETQ SENTLIST (ENTGET EN))
(SETQ XDATA (LIST (LIST -3 (LIST "LANDXML" (CONS 1000 xdatai)))))
   (setq NEWSENTLIST (APPEND SENTLIST XDATA))
  (ENTMOD NEWSENTLIST)
 ));p&if object has xdata
    (setq count (+ count 1))
    );r
  
);defun






;Tan function
(defun tan (ang)
 (/ (sin ang)(cos ang))
 )

 (defun LL2MGA (/)

   (setq a 6378137)
    (setq ee2 0.00669438)
   (setq ee4 (* ee2 ee2))
   (setq ee6 (* ee2 ee4))
    (setq k0 0.9996)

    ;(setq p1 (getpoint))

    (setq LAT (cadr p1) )
    (setq latrad (* lat (/ pi 180.0)))
(setq LONG (car p1))
    (setq longrad (* long (/ pi 180.0)))

   (setq sino (SIN latrad))
   (setq sin2o (SIN (* 2 latrad)))
   (setq sin4o (sin (* 4 latrad)))
   (setq sin6o (sin (* 6 latrad)))
   
    (setq longorigin (- (* (atof (rtos (/ (+  long 3) 6) 2 0)) 6) 3))
    (setq longoriginrad (* longorigin (/ pi 180.0)))
   (SETQ longdiffrad (- longrad longoriginrad))

   (setq A0 (- 1 (/ ee2 4) (/ (* 3 ee4) 64) (/ (* 5 ee6) 256)))
   (SETQ A2  (* (/ 3.0 8.0)  (+ ee2 (/ ee4 4) (/ (* 15 ee6) 128))))
   (SETQ A4 (* (/ 15.0 256.0) (+ ee4 (/ (* 3 ee6) 4 ))))
   (SETQ A6 (/ (* 35 ee6) 3072))

   (SETQ M1 (* a A0 latrad))
   (SETQ M2 (* -1 a A2 sin2o))
   (SETQ M3 (* a A4 sin4o))
   (SETQ M4 (* -1 a A6 sin6o))
   (setq sumM (+ M1 M2 M3 M4))

   (SETQ RHO (* a (/ (- 1 ee2)(expt (- 1 (* ee2 sino sino))1.5) )))
   (SETQ NU (/ a (expt (- 1 (* ee2 sino sino)) 0.5)))

   (setq P1 (COS LATRAD))
   (SETQ P2 (EXPT P1 2))
   (SETQ P3 (EXPT P1 3))
   (SETQ P4 (EXPT P1 4))
   (SETQ P5 (EXPT P1 5))
   (SETQ P6 (EXPT P1 6))
   (SETQ P7 (EXPT P1 7))

   (SETQ L2 (EXPT longdiffrad 2))
   (SETQ L3 (EXPT longdiffrad 3))
   (SETQ L4 (EXPT longdiffrad 4))
   (SETQ L5 (EXPT longdiffrad 5))
   (SETQ L6 (EXPT longdiffrad 6))
   (SETQ L7 (EXPT longdiffrad 7))
   (SETQ L8 (EXPT longdiffrad 8))

   (SETQ TLAT (TAN LATRAD))
   (SETQ T2 (EXPT TLAT 2))
   (SETQ T4 (EXPT TLAT 4))
   (SETQ T6 (EXPT TLAT 6))

   (SETQ PSI (/ NU RHO))
   (SETQ PSI2 (EXPT PSI 2))
   (SETQ PSI3 (EXPT PSI 3))
   (SETQ PSI4 (EXPT PSI 4))

   (SETQ E1 (* NU longdiffrad P1))
   (SETQ E2 (/ (* NU L3 P3 (- PSI T2)) 6.0))
   (SETQ E3 (/ (* NU L5 P5 (+ (- (+ (* 4.0 PSI3 (- 1 (* 6.0 T2))) (* PSI2 (+ 1.0 (* 8.0 T2)))) (* PSI (* 2 T2))) T4)) 120.0))
   (SETQ E4 (/ (* RHO L7 P7 (- (+ (- 61.0 (* 479.0 T2)) (* 179.0 T4)) T6)) 5040.0))
   (SETQ SUME (+ E1 E2 E3 E4))
   (SETQ SUMEK0 (* SUME k0))
   (SETQ EAST (+ 500000.0 SUMEK0))

   (SETQ N1 (/ (* NU sino L2 P1) 2))
   (SETQ N2 (/ (* NU sino L4 P3 (- (+ (* 4.0 PSI2) PSI) T2)) 24.0))
   (SETQ N3 (/ (* RHO sino L6 P5 (+ (- (+ (- (* 8 PSI4 (- 11 (* 24 T2))) (* 28 PSI3 (- 1 (* 6 T2)))) (* PSI2 (- 1 (* 32 T2)))) (* PSI (* 2 T2))) T4)) 720))
   (SETQ N4 (/ (* RHO sino L8 P7 (- (+ (- 1385 (* 3111 T2)) (* 543 T4)) T6)) 40320))
   (SETQ SUMN (+ SUMM N1 N2 N3 N4))
   (SETQ SUMNK0 (* SUMN k0))
   (SETQ NORTH (+ SUMNK0 10000000.0))
	 
)  






;-------------------------------------------------------loops checker FOR GPACK DWGS----------------------------------------
(defun C:XLCD (/)
  (XLCD)
  )
       

(defun XLCD (/)


  
    (setq cgpl (list));EMPTY CGPOINTLIST
  (setq rolist (list));EMPTY Reference list
  (setq obslist (list));empty observation list
  (setq monlist (list));Empty Monument list
  (setq lotlist (list));Empty lot definition list
  (setq arclist (list));empty observed arc list
  (setq dpl (List));empty datum point list
  (setq pmlist (list));empty pm list
  (setq mplist (list));empty multipartlist
  (setq mpalist (list));empty mulitplart assignment list
  (setq mpolist (list));empty multipart output list
  (setq pflist (list));empty plan features list
  (setq pcount 0);set point count number to 0
  (setq rocount 0);set ref obs count to 0
  (setq moncount 0);set monument count to 0
  (setq cogeocount 1);set coordinate geometry count to 1
  (setq pfwcount 1);set plan features wall count to 1
  (setq pfkcount 1);set plan features kerb count to 1
  (setq pffcount 1);set plan features fence count to 1
  (setq pfbcount 1);set plan features building count to 1
  (setq iblselist (list));irregular boundary start and end points
  (setq ibllist (list));irregular boundary line list for polyline shuffler
  (setq islist (list));instrument station list
  (setq flowarrows "")
  (setq annolist "")
  (setq Roadcount 1)
  (setq Easecounta 1)
    (setq annocount 1)
    (setq easelinklist (list))
    (setq linkno 1)
    (setq resdesclist (list));restriction link list
    (setq reslinklist (list));restriction description list
    (setq oclist (list));list of owners corp entitlements
    (setq distlist (list));list of distance being exported to dynanet so each distance is exported once
  

  
 
      
  

    ;3.get adjustment lines
    (princ "\nProcessing Observation Lines ")
 
(IF (/= (setq bdyline (ssget '((0 . "LINE,ARC") (8 . "Normal,Boundary,Topo,Traverse,Sideshot")))) nil)(progn 
  (setq count 0)
  (repeat (sslength bdyline)

    (IF (= (CDR(ASSOC 0 (ENTGET (SSNAME bdyline COUNT)))) "LINE")
      (PROGN
	
    
  (SETQ P1 (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ P2 (CDR(ASSOC 11 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ LAYER (strcase (CDR(ASSOC 8 (ENTGET (SSNAME bdyline COUNT)))) T))

    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	   (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nBoundary Line with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
       (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))

   
    (if (= (setq remlist (member p1s cgpl)) nil)(progn
						 (if (setq remlist1 (member p1s dcdbpl))
						   (progn
						     (setq outnum (cadr remlist1)
							   pointtype "DCDB")
						     )
						     (setq outnum(rtos (+ pcount 1) 2 0)
							   pointtype "Surplus"
							   pcount (+ pcount 1))
						   
						   )
						     
				   (setq is1 outnum)
				   (setq cgpl (append cgpl (list p1s)(list "boundary")(list pointtype)(list outnum)))
				   );p
;else
      (setq is1 (nth 3 remlist))
       );&if
				   
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (if (setq remlist1 (member p2s dcdbpl))
						   (progn
						     (setq outnum (cadr remlist1)
							   pointtype "DCDB")
						     )
						     (setq outnum (rtos (+ pcount 1) 2 0)
							   pointtype "Surplus"
							   pcount (+ pcount 1))
						   )
						     
				   (setq is2 outnum)
				   (setq cgpl (append cgpl (list p2s)(list "boundary")(list pointtype)(list outnum)))
				   );p
;else
      (setq is2 (nth 3 remlist))
       );&if

    (if (= layer "boundary")(setq layer "normal"))

    (setq rocount (+ rocount 1))
    (setq roline (strcat "<ReducedObservation name=\"OBS-" (rtos rocount 2 0) "\" purpose=\""LAYER "\" setupID=\"" is1 "\" targetSetupID=\"" is2 "\" " xdatai))
    (setq rolist (append rolist (list roline)))
    (setq obslist (append obslist (list (strcat is1 "-" is2))(list ( strcat is2 "-" is1))))
    (setq islist (append islist (list is1 is2)))

  );P IS LINE

      (PROGN ; IS AN ARC


    
(SETQ CP (CDR(ASSOC 10 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ RADIUS (CDR(ASSOC 40 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG1 (CDR(ASSOC 50 (ENTGET (SSNAME bdyline COUNT)))))
  (SETQ ANG2 (CDR(ASSOC 51 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ EN (CDR(ASSOC -1 (ENTGET (SSNAME bdyline COUNT)))))
    (SETQ LAYER (strcase (CDR(ASSOC 8 (ENTGET (SSNAME bdyline COUNT)))) T ))

  (SETQ P1 (POLAR CP ANG1 RADIUS))
  (SETQ P2 (POLAR CP ANG2 RADIUS))



    	    (SETQ XDATAI (ENTGET EN '("LANDXML")))
	   (IF (= (SETQ XDATAI (ASSOC -3 XDATAI)) NIL) (progn
	      	     (COMMAND "CHANGE" en "" "P" "C" "6" "")
	      (princ (strcat "\nBoundary Line with no XML data at " (rtos (car p1) 2 3) "," (rtos (cadr p1)2 3)))
	     ))
	    (SETQ XDATAI (NTH 1 XDATAI))
	    (SETQ XDATAI (CDR (NTH 1 XDATAI)))
       (setq &pos 0)
	      (while (/=  (setq &pos (vl-string-search "&" xdatai &pos )) nil) (setq xdatai (vl-string-subst "&amp;" "&"  xdatai &pos)
										      &pos (+ &pos 4)))

    
    (setq p1s (strcat (rtos (cadr p1) 2 4) " " (rtos (car p1) 2 4)))
    (setq p2s (strcat (rtos (cadr p2) 2 4) " " (rtos (car p2) 2 4)))

   
    (if (= (setq remlist (member p1s cgpl)) nil)(progn
						 (if (setq remlist1 (member p1s dcdbpl))
						   (progn
						     (setq outnum (cadr remlist1)
							   pointtype "DCDB")
						     )
						     (setq outnum(rtos (+ pcount 1) 2 0)
							   pointtype "Surplus"
							   pcount (+ pcount 1))
						   
						   )
						     
				   (setq is1 outnum)
				   (setq cgpl (append cgpl (list p1s)(list "boundary")(list pointtype)(list outnum)))
				   );p
;else
      (setq is1 (nth 3 remlist))
       );&if
				   
     (if (= (setq remlist(member p2s cgpl)) nil)(progn
				   (if (setq remlist1 (member p2s dcdbpl))
						   (progn
						     (setq outnum (cadr remlist1)
							   pointtype "DCDB")
						     )
						     (setq outnum (rtos (+ pcount 1) 2 0)
							   pointtype "Surplus"
							   pcount (+ pcount 1))
						   )
						     
				   (setq is2 outnum)
				   (setq cgpl (append cgpl (list p2s)(list "boundary")(list pointtype)(list outnum)))
				   );p
;else
      (setq is2 (nth 3 remlist))
       );&if

;check for clockwise rotation
    (if (/= (setq stringpos (vl-string-search "rot" xdatai )) nil)(progn
(setq wwpos (vl-string-position 34 xdatai (+ stringpos 5)))(setq rot (substr xdatai (+ stringpos 6) (-(- wwpos 1)(+ stringpos 4))))))

    (if (= rot "cw")(progn
		      (setq is1r is1
			is1 is2
			is2 is1r)
		      
      ))

    ;turn arc into line
    
    (setq stringpos (vl-string-search "chordAzimuth" xdatai ))
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 14)))
            (setq bearing (substr xdatai (+ stringpos 15) (-(- wwpos 1)(+ stringpos 13))))
	    (setq xbearing bearing)

	    (setq stringpos (vl-string-search "length" xdatai ))
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 8)))
            (setq arclength (substr xdatai (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))
	    (setq arclength (rtos (atof arclength)2 3));remove trailing zeros

	    (setq stringpos (vl-string-search "radius" xdatai ))
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 8)))
            (setq radius (substr xdatai (+ stringpos 9) (-(- wwpos 1)(+ stringpos 7))))

 ;           (if (setq stringpos (vl-string-search "angleAccClass" xdatai ))(progn
;	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 15)))
 ;           (setq sdb (substr xdatai (+ stringpos 16) (-(- wwpos 1)(+ stringpos 14)))))(setq sdb ""))

;	    (if (setq stringpos (vl-string-search "distanceAccClass" xdatai ))(progn
;	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 18)))
;            (setq sdd (substr xdatai (+ stringpos 19) (-(- wwpos 1)(+ stringpos 17)))))(setq sdd ""))

            (if (/= (setq stringpos (vl-string-search "desc" xdatai )) nil)(progn
	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 6)))
            (setq desc (substr xdatai (+ stringpos 7) (-(- wwpos 1)(+ stringpos 5))))
	    )(setq desc ""))

  ;          (setq stringpos (vl-string-search "refplan" xdatai ))
;	    (setq wwpos (vl-string-position 34 xdatai (+ stringpos 9)))
  ;          (setq refplan (substr xdatai (+ stringpos 10) (-(- wwpos 1)(+ stringpos 8))))

    ;calc chord distance, note using string values not digital values
	    (setq stringO (/ (atof arclength) (atof radius)));arc internal angle based on string values
	    (setq dist (rtos (* 2 (atof radius) (sin (/ stringO 2))) 2 3))

 ;       (if (/= desc "")(setq desc (strcat " desc=\"" desc "\"")))
;    (if (/= sdb "")(setq sdb (strcat " angleAccClass=\"" sdb "\"")))
;    (if (/= sdd "")(setq sdd (strcat " distanceAccClass=\"" sdd "\"")))

    
;(SETQ xdatai (STRCAT "azimuth=\"" xbearing "\" horizDistance=\"" dist "\" refplan=\"" refplan "\"" sdb sdd desc ))
    (SETQ xdatai (STRCAT "azimuth=\"" xbearing "\" horizDistance=\"" dist  "\"" desc ))

    (if (= layer "boundary")(setq layer "normal"))
    
    (setq rocount (+ rocount 1))
    (setq roline (strcat "<ReducedObservation name=\"OBS-" (rtos rocount 2 0) "\" purpose=\"" LAYER"\" setupID=\"" is1 "\" targetSetupID=\"" is2 "\" " xdatai))
    (setq rolist (append rolist (list roline)))
    (setq obslist (append obslist (list (strcat is1 "-" is2))(list ( strcat is2 "-" is1))))
    (setq islist (append islist (list is1 is2)))


    ;record backwards observation

));P IS ARC

    
    (setq count (+ count 1))
    );r
  );p
  );if

  (setq rolist (append rolist (list "</ObservationGroup>")))

  (if (/= obslist nil)
    (progn
  (SETQ XLCCHECKER "D")
  (SETQ SIMPLESTOP "1")
  (OBSIMPORTER)
  ))
   
  (SETQ XLCCHECKER "N")

    )





    
	  
	  
      