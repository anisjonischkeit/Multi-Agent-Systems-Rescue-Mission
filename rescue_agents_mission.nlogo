__includes ["communication.nls" "bdi.nls"]

breed [ civilians civilian]
breed [ obstacles obstacle]
breed [ rescue-units rescue-unit]
breed [ rescued rescue-1]
breed [ bases base]
breed [ ambulances ambulance]

globals [rescued-cvls picked-up distance-traveled]
ambulances-own [beliefs intentions incoming-queue load]
bases-own [incoming-queue]

;;; Setting up the environment
to setup
   ;; (for this model to work with NetLogo's new plotting features,
   ;; __clear-all-and-reset-ticks should be replaced with clear-all at
   ;; the beginning of your setup procedure and reset-ticks at the end
   ;; of the procedure.)
   ;;__clear-all-and-reset-ticks
   clear-all
   reset-ticks
   random-seed seed
   setup-civilians
   setup-obstacles
   setup-ambulances
   setup-rescue-units
   setup-base
   set rescued-cvls 0
   set distance-traveled 0
   set picked-up 0
end

;;; creating disaster victims (civilians)
to setup-civilians
   create-civilians num-victims [
     rand-xy-co
     set shape "person"
     set color red
   ]
end

;;; creating obstacles
to setup-obstacles
   create-obstacles num-obstacles [
     rand-xy-co
     set shape "box"
     set color yellow
   ]
end

;;; creating ambulances
to setup-ambulances
   create-ambulances num-ambulances [
     rand-xy-co
     set shape "abulance"
     set color red
     set beliefs []
     set intentions []
     set incoming-queue []
     set load 0
     add-intention "do-nothing" timeout_expired 30
   ]
end
;;; creating Rescue units
to setup-rescue-units
   create-rescue-units num-rescue-units [
     rand-xy-co
     set shape "rescue-unit"
     set color blue
   ]
end

to setup-base
   create-bases 1 [
     set shape "triangle 2"
     set color red
     setxy 0 0
     set incoming-queue []
   ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; END of SETUP

;;;; Experiment
to run-rescue
   if count civilians = 0 and count rescued = 0 [stop]
   ask bases [base-behaviour]
   ask ambulances [ambulance-behaviour]
   ask rescue-units [rescue-unit-behaviour]
   tick
end










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to base-behaviour
   let msg 0
   let performative 0

   while [not empty? incoming-queue]
   [
      set msg get-message
      set performative get-performative msg
      if performative = "inform" [allocate-the-rescue msg]
   ]
end

to allocate-the-rescue [msg]
   let coords (item 1 get-content msg)
   broadcast-to ambulances add-content (list "collect" coords) create-message "request"
end












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;
;;; Ambulance Agent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hybrid Layer.
to ambulance-behaviour
   if reactive-ambulance-unit [stop]
   collect-msg-update-intentions-unit
   execute-intentions
end

;;; Reactive layer of ambulance Agent.
to-report reactive-ambulance-unit
   if detect-ambulance [avoid-obstacle report true]
   if load >= maximum_load and at-dest base-id [set load 0] ;; unload patients
   if load >= maximum_load [move-towards-dest base-id report true]
   if detect-civilian [rescue-civilian pick-up-victim report true]
   report false
end

;;; Ambulance unit proactive behaviour
to collect-msg-update-intentions-unit
   let msg 0
   let performative 0

   while [not empty? incoming-queue]
   [
     set msg get-message
     set performative get-performative msg
     if performative = "request" [add-belief get-content msg]
     if performative = "found" [
       show "found"
       show msg
       show get-content msg
       show beliefs
       remove-belief get-content msg
       show intentions
       show beliefs
     ]
     if performative = "remove-intention" [
       show get-intention
       let intent-to-remove get-content msg
       set intentions remove intent-to-remove intentions
     ]
   ]


   if exist-beliefs-of-type "collect" and empty? intentions [
       let bel closer beliefs-of-type "collect"
       let coords item 1 bel
       remove-belief bel
       add-intention "pick-up-victim" "true"
       add-intention (word "move-towards-dest " coords) (word "at-dest " coords)
    ]
end



;;; Reports the closest item in list.
;;; This reports the closer to the agent item from a list of items. The coordinates of the
;;; different members in the list of items must be in a list as well. For example
;;; the list must be of the form [ ["collect" [12 3] ["collect" [14 7]]]
to-report closer [itemlist]
   let closest first itemlist
   foreach itemlist
   [
      if distance-coords (item 1 ?) < distance-coords (item 1 closest)
        [set closest ?]
   ]
   report closest
end









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rescue Units
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to rescue-unit-behaviour
   if detect-civilian [rescue-civilian inform-base stop]
   if detect-obstacle [avoid-obstacle stop]
   if true [move-randomly]
end

;;; Informing base for victim
;;; creates a message for the location of the victim, where the content is
;;; "victim-at" [xcor ycor]
to inform-base
   send add-receiver base-id add-content (list "victim-at" (list (round xcor) (round ycor))) create-message "inform"
end











;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Sensors
;; Detecting obstacles
;; Obstacles are obstacles and other rescue agents.
to-report detect-obstacle
   foreach (list patches in-cone 2 30)
   [
     if any? obstacles-on ? [show "obstacle-on" report true]
     if any? other rescue-units-on ? [show "rescue-on" report true]
   ]
   report false
end

to-report detect-ambulance
   foreach (list patches in-cone 2 30)
   [
     if any? other ambulances-on ? [report true]
   ]
   report false
end

;;; detecting a civilian
to-report detect-civilian
   ifelse any? civilians-here
   [report true]
   [report false]
end

;;;; Returns true if an agent is at the specific destination.
to-report at-dest [dest]
   if is-number? dest [
      ifelse ([who] of one-of turtles-here = dest)
      [report true]
      [report false]
    ]

    if is-list? dest [
      ifelse (abs (xcor - first dest) < 0.4 ) and (abs (ycor - item 1 dest) < 0.4)
      [report true]
      [report false]
    ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rescueing a civilian
to rescue-civilian
   set rescued-cvls rescued-cvls + 1
   ask one-of civilians-here [
      set breed rescued
      set shape "person"
      set color green
   ]
end

;;; Actions that move the agent around.
;;; Turning randomly to avod an obstacle
to avoid-obstacle
   set heading heading + random 360
end

;; moving randomly. First move then turn
to move-randomly
   fd 1
   set heading heading + random 30 - random 30
end

;;;;;;;;;;;;;;;;;
to pick-up-victim
   ask rescued-here [die]
   set picked-up picked-up + 1
   set load load + 1
   broadcast-to ambulances add-content (list "collect" (list (round xcor) (round ycor))) create-message "found"
   broadcast-to ambulances add-content intentions create-message "remove-intention"
end

;;; Top level Reactive-traveling.
to move-towards-dest [dest]
   if true [travel-towards dest stop]
end

;;; Traveling towars a destination.
to travel-towards [dest]
    fd 0.2
    set distance-traveled distance-traveled + 0.2
    if is-number? dest
    [
      if not ((xcor = [xcor] of turtle dest) and (ycor = [ycor] of turtle dest))
      [
        set heading towards-nowrap turtle dest
      ]
    ];; safe towards

    if is-list? dest
    [
      if not ((xcor = first dest) and (ycor = item 1 dest))
      [
        set heading towardsxy-nowrap (first dest) (item 1 dest)
      ]
    ];; safe towards
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
to rand-xy-co
   let x 0
   let y 0

   loop [
      set x random-pxcor
      set y random-pycor
      if not any? turtles-on patch x y and not (abs x < 4 and abs y < 4) [setxy x y stop]
   ]
end

;;; Reports the distance from a set of coordinates [x y] that are given in  a list eg [3 4]
to-report distance-coords [crds]
   report distancexy-nowrap (first crds) (item 1 crds)
end

;;; base ID is required to broadcasy a message to the base.
;;; This is intended for use with the add-receiver reporter.
to-report base-id
   report first [who] of bases
end
@#$#@#$#@
GRAPHICS-WINDOW
481
10
1054
547
19
17
14.46
1
10
1
1
1
0
1
1
1
-19
19
-17
17
1
1
1
ticks
30.0

SLIDER
256
67
428
100
num-victims
num-victims
0
100
29
1
1
NIL
HORIZONTAL

BUTTON
20
17
83
50
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
256
106
428
139
num-obstacles
num-obstacles
0
50
31
1
1
NIL
HORIZONTAL

SLIDER
256
210
428
243
num-ambulances
num-ambulances
1
10
7
1
1
NIL
HORIZONTAL

SLIDER
256
172
428
205
num-rescue-units
num-rescue-units
0
30
10
1
1
NIL
HORIZONTAL

BUTTON
96
17
190
50
NIL
run-rescue
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
195
18
289
51
NIL
run-rescue
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
81
288
138
333
Saved
rescued-cvls
0
1
11

MONITOR
11
288
78
333
In-Danger
count civilians
0
1
11

SWITCH
276
323
428
356
show-intentions
show-intentions
0
1
-1000

SWITCH
276
359
428
392
show_messages
show_messages
0
1
-1000

MONITOR
140
288
204
333
Collected
picked-up
3
1
11

SLIDER
8
171
180
204
seed
seed
0
100
44
1
1
NIL
HORIZONTAL

MONITOR
12
340
122
385
NIL
distance-traveled
3
1
11

TEXTBOX
10
88
160
163
Keep the seed tidentical in the same experiments in order to compare accurately models (Deterministic behaviour of models).
11
0.0
0

CHOOSER
289
270
427
315
maximum_load
maximum_load
1 2 5 8 10
1

@#$#@#$#@
Rescue Agents

Assignment exercise coutesy of I. Sakellariou
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

abulance
true
0
Rectangle -1184463 true false 45 45 255 255
Rectangle -13345367 true false 75 15 225 75
Line -13345367 false 45 90 150 90
Line -13345367 false 150 90 255 90
Rectangle -6459832 true false 30 75 60 135
Rectangle -6459832 true false 30 165 60 225
Rectangle -6459832 true false 240 75 270 135
Rectangle -6459832 true false 240 165 270 225
Circle -2674135 true false 118 43 62
Rectangle -7500403 true true 105 120 195 255

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 30 30 240

circle 2
false
0
Circle -7500403 true true 16 16 270
Circle -16777216 true false 46 46 210

circle 3
true
0
Circle -7500403 false true 0 0 300

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

rescue-unit
true
0
Rectangle -7500403 true true 60 45 240 270
Circle -2674135 true false 74 104 152
Rectangle -1 true false 135 135 150 210
Rectangle -1 true false 105 165 180 180
Rectangle -1184463 true false 75 15 105 105
Rectangle -1184463 true false 135 15 165 90
Rectangle -1184463 true false 195 15 225 105

scouter
true
0
Rectangle -1184463 true false 45 75 120 240
Rectangle -1184463 true false 180 75 255 240
Rectangle -7500403 true true 120 30 180 285
Rectangle -16777216 true false 60 90 105 105
Rectangle -16777216 true false 60 120 105 135
Rectangle -16777216 true false 60 150 105 165
Rectangle -16777216 true false 60 180 105 195
Rectangle -16777216 true false 60 210 105 225
Rectangle -16777216 true false 195 210 240 225
Rectangle -16777216 true false 195 180 240 195
Rectangle -16777216 true false 195 150 240 165
Rectangle -16777216 true false 195 120 240 135
Rectangle -16777216 true false 195 90 240 105

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 60 270 150 0 240 270 15 105 285 105
Polygon -7500403 true true 75 120 105 210 195 210 225 120 150 75

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

simple
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0

@#$#@#$#@
0
@#$#@#$#@
