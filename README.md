Constructors and combinators for `pict`

This library provides some extra support for constructing and combining
`pict`s. It originated as a helper library for several of my
presentations, and since I’ve had some reuse with them, so might others.

```racket
(list*of contract) -> contract?
  contract : contract?         
```

The "contract" constructor that is alluded to by the documentation for
`table`. It is an improper list contract, where the last cons pair’s
`cdr` may be `'()` or a value satisfying `contract`.

```racket
Examples:                                           
> (andmap (list*of number?) '(3 (0 1) (0 1 . 2) ()))
#t                                                  
> ((list*of number?) '(0 . nope))                   
#f                                                  
```

```racket
(colorize-if test pict color) -> pict?
  test : any/c                        
  pict : pict?                        
  color : color/c                     
```

A useful pattern: `(if test (colorize pict color) pict)`

```racket
(pin-over-center base x y pict) -> pict?
  base : pict?                          
  x : real?                             
  y : real?                             
  pict : pict?                          
```

Pin the center of `pict` to `x` and `y` over `base`.

```racket
(pin-over-hcenter base x y pict) -> pict?
  base : pict?                           
  x : real?                              
  y : real?                              
  pict : pict?                           
```

Like `pin-over-center`, only centers the x-axis.

```racket
(pin-over-vcenter base x y pict) -> pict?
  base : pict?                           
  x : real?                              
  y : real?                              
  pict : pict?                           
```

Like `pin-over-hcenter`, but for the y-axis.

```racket
(both fn) -> void?        
  fn : (-> boolean? void?)
```

To be used with `slide`-producing functions that have only two modes,
signified by `#t` and `#f`.

```racket
(pin-under-all base tag pict) -> pict?
  base : pict?                        
  tag : symbol?                       
  pict : pict?                        
```

Center `pict` under all picts that are tagged (with `tag-pict`) with
`tag` in `base`.

```racket
(pin-over-tag base finder tag wrap) -> pict?         
  base : pict?                                       
  finder : (-> pict? pict-path? (values real? real?))
  tag : symbol?                                      
  wrap : (-> pict? pict?)                            
```

Find a pict in `base` tagged `tag`, apply `wrap` to the found pict and
pin over `base` at the coordinates given by `finder`.

```racket
(pin-under-tag base finder tag wrap) -> pict?        
  base : pict?                                       
  finder : (-> pict? pict-path? (values real? real?))
  tag : symbol?                                      
  wrap : (-> pict? pict?)                            
```

Like `pin-over-tag`, but uses `pin-under`.

```racket
(thick-ellipse  w                                 
                h                                 
                border-width                      
                color                             
               [#:fill-color fill-color]) -> pict?
  w : nonneg-real?                                
  h : nonneg-real?                                
  border-width : (real-in 0 255)                  
  color : color/c                                 
  fill-color : (or/c #f color/c) = #f             
```

Like `ellipse/border`, only uses the pen to draw a border rather than
layer different colored ellipses. This produces more consistent borders.

```racket
(thick-filled-rounded-rectangle  w                            
                                 h                            
                                [corner-radius                
                                 #:color color                
                                 #:style style                
                                 #:angle angle                
                                 #:border-width border-width  
                                 #:border-color border-color] 
                                 #:border-style border-style) 
 -> pict?                                                     
  w : nonneg-real?                                            
  h : nonneg-real?                                            
  corner-radius : real? = -0.25                               
  color : color/c = "black"                                   
  style : brush-style/c = 'solid                              
  angle : real? = 0                                           
  border-width : (real-in 0 255) = 1                          
  border-color : (or/c #f color/c) = #f                       
  border-style : pen-style/c                                  
```

Like `filled-rounded-rectangle`, but adds a border with a pen. Can
additionally rotate the rectangle by `angle`.

```racket
(filled-rounded-rectangle-frame  pict                          
                                [#:corner-radius corner-radius 
                                 #:scale scale                 
                                 #:x-scale x-scale             
                                 #:y-scale y-scale             
                                 #:color color                 
                                 #:angle angle                 
                                 #:border-width border-width   
                                 #:border-color border-color]  
                                 #:border-style border-style)  
 -> pict?                                                      
  pict : pict?                                                 
  corner-radius : real? = -0.25                                
  scale : nonneg-real? = 1                                     
  x-scale : nonneg-real? = 1                                   
  y-scale : nonneg-real? = 1                                   
  color : color/c = "white"                                    
  angle : real? = 0                                            
  border-width : (real-in 0 255) = 1                           
  border-color : (or/c #f color/c) = #f                        
  border-style : pen-style/c                                   
```

Uses `thick-filled-rounded-rectangle` to form a frame around a given
pict, and gives the ability to scale uniformly and with each dimension.
The x-axis and y-axis are scaled by `(* scale x-axis)` and `(* scale
y-axis)` respectively.

```racket
(filled-flash-frame  pict                                    
                    [#:scale scale                           
                     #:corner-radius corner-radius           
                     #:outline outline                       
                     #:n-points n-points                     
                     #:spike-fraction spike-fraction         
                     #:rotation rotation])           -> pict?
  pict : pict?                                               
  scale : nonneg-real? = 1                                   
  corner-radius : real? = -0.25                              
  outline : (or/c #f color/c) = #f                           
  n-points : exact-positive-integer? = 10                    
  spike-fraction : (real-in 0 1) = 1                         
  rotation : real? = 0                                       
```

Use `slideshow/flash` to produce a frame around a given pict. If
`outline` not `#f`, then additionally draws an outlined flash with the
color bound to `outline`.

```racket
(play-n-at n stage stages picts ghost-rest?) -> (listof pict?)
  n : exact-nonnegative-integer?                              
  stage : exact-nonnegative-integer?                          
  stages : (nondecreasing-listof exact-nonnegative-integer?)  
  picts : (listof pict?)                                      
  ghost-rest? : boolean?                                      
```

Chunks `picts` into strides of `n`, for each number in `stages`. If
`stage` is less than or equal to a stage given in `stages`, then that
stride is present in the output. If not, either stop and produce an
empty tail, or ghost the rest of the picts in the tail, depending on
`ghost-rest?`.

```racket
(progressive-table  stage                                   
                    stages                                  
                    ncols                                   
                    picts                                   
                    col-aligns                              
                    row-aligns                              
                    col-seps                                
                    row-seps                                
                   [#:ghost? ghost?]) -> pict?              
  stage : exact-nonnegative-integer?                        
  stages : (nondecreasing-listof exact-nonnegative-integer?)
  ncols : exact-nonnegative-integer?                        
  picts : (listof pict?)                                    
  col-aligns : (list*of (pict? pict? . -> . pict?))         
  row-aligns : (list*of (pict? pict? . -> . pict?))         
  col-seps : (list*of real?)                                
  row-seps : (list*of real?)                                
  ghost? : boolean? = #t                                    
```

An interface for staging rows in a `table`, that uses `play-n-at` to
produce the picts for `table`.

```racket
Examples:                                                        
> (define (foo stage)                                            
   (define (angles p) (hc-append (t "〈") p (t "〉")))             
   (progressive-table stage (list 0 0 1 2) 2                     
                      (list (angles (code x ρ₁ σ₂)) (code 1)     
                            (angles (code (f y) ρ₁ σ₁)) (code 1) 
                            (angles (code x ρ₅ σ₅)) (code 2)     
                            (angles (code (f y) ρ₄ σ₄)) (code 2))
                      lc-superimpose cc-superimpose gap-size 5)) 
                                                                 
> (foo 0)                                                        
                                                                 
> (foo 1)                                                        
                                                                 
> (foo 2)                                                        
                                                                 
```
