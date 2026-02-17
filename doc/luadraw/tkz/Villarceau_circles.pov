//luadraw 2.6
//options: +W640 +H492 -V +A +FN
#version 3.7;
global_settings { charset utf8
     ambient_light rgb 1.5
     assumed_gamma 1.8}
background{ color rgb<0.8275,0.8275,0.8275> }
camera{ orthographic
     location 17320.508076*<-0.851651,0.309976,0.422618>
     sky <0.397131,-0.144544,0.906308>
     up 10*y
     right 13*x
     look_at <0,0,0>
     translate <0,0,0>}
light_source { 17320.508076*<-0.729022,-0.058813,0.681959> color rgb<1,1,1>}

//declarations

#declare object1 = torus{
    3, 1
    matrix <1,0,0,
            0,0,1,
            0,-1,0,
            0,0,0>
  clipped_by { plane{ <-0.333333,0,0.942809>, 0 } }
  }

#declare object2 = mesh2{
         vertex_vectors{ 4,
          <-4.5,4.5,-1.59099>
          ,<-4.5,-4.5,-1.59099>
          ,<4.5,-4.5,1.59099>
          ,<4.5,4.5,1.59099>
                       }
         face_indices{ 2,
           <0,1,2>
           ,<0,2,3>
                       }
  }

#declare object3 =  union{
    cylinder{<-4.5,-4.5,-1.59099>,<4.5,-4.5,1.59099> 0.014058}
    cylinder{<4.5,-4.5,1.59099>,<4.5,4.5,1.59099> 0.014058}
    cylinder{<4.5,4.5,1.59099>,<-4.5,4.5,-1.59099> 0.014058}
    cylinder{<-4.5,4.5,-1.59099>,<-4.5,-4.5,-1.59099> 0.014058}
  }

#declare object4 =  union{
    cone{ <-4.732825,0,0> 0.089058
           <-5,0,0> 0 }
    cylinder{<5,0,0>,<-4.732825,0,0> 0.014058}
    cone{ <0,4.732825,0> 0.089058
           <0,5,0> 0 }
    cylinder{<0,-5,0>,<0,4.732825,0> 0.014058}
    cone{ <0,0,4.732825> 0.089058
           <0,0,5> 0 }
    cylinder{<0,0,-5>,<0,0,4.732825> 0.014058}
  }

#declare object5 =  
    sphere{<0,0,0> 0.1125
  }

#declare object6 = torus{
    3, 0.042175
    matrix <0.888889,0.333333,0.31427,
            -0.333333,0,0.942809,
            0.31427,-0.942809,0.111111,
            0,1,0>
  }

#declare object7 = torus{
    3, 0.042175
    matrix <0.888889,0.333333,0.31427,
            -0.333333,0,0.942809,
            0.31427,-0.942809,0.111111,
            0,-1,0>
  }

//renderings

object{ object1
  texture{ 
         pigment{ color rgbt<0.2745,0.5098,0.7059,0>}
         finish{ ambient 0.35 diffuse 0.8 phong 0.5} }
      }

object{ object2
  texture{ 
         pigment{ color rgbt<0.1804,0.5451,0.3412,0.6>}
         finish{ ambient 0.35 diffuse 0.8 phong 0.5} }
      }

object{ object3
  texture{ 
         pigment{ color rgbt<0,0,0,0>}
         finish{ ambient 0.35 diffuse 0.8 phong 0.5} }
      }

object{ object4
  texture{ 
         pigment{ color rgbt<1,0.8431,0,0>}
         finish{ ambient 0.35 diffuse 0.8 phong 0.5} }
      }

object{ object5
  texture{ 
         pigment{ color rgbt<0,0,0,0>}
         finish{ ambient 0.35 diffuse 0.8 phong 0.5} }
      }

object{ object6
  texture{ 
         pigment{ color rgbt<0.8627,0.0784,0.2353,0>}
         finish{ ambient 0.35 diffuse 0.8 phong 0.5} }
      }

object{ object7
  texture{ 
         pigment{ color rgbt<0.8627,0.0784,0.2353,0>}
         finish{ ambient 0.35 diffuse 0.8 phong 0.5} }
      }

