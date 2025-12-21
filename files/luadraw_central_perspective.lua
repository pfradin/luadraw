-- luadraw_central_perspective.lua 
-- date 2025/12/21
-- version 2.4
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

--functions to be redefined
local old_circle3db = circle3db
local old_arc3db = arc3db
local old_Dcone = graph3d.Dcone
local old_Dcylinder = graph3d.Dcylinder
local old_Dfrustum = graph3d.Dfrustum
local old_Dsphere = graph3d.Dsphere
local old_Proj3d = graph3d.Proj3d
local old_Proj3dV = graph3d.Proj3dV
local old_Cosine_incidence = graph3d.Cosine_incidence
local old_Isvisible = graph3d.Isvisible
local old_Screenpos = graph3d.Screenpos
local old_Observer_distance = graph3d.Observer_distance

camera = nil
target = Origin

--This function is automatically called by the perspective function.
function central_perspective(theta,phi,d,look) -- or central_perspective(camera, look)
    local N
    local cos, sin, tet, ph = math.cos, math.sin
    if isPoint3d(theta) then
        camera = theta
        target = phi or Origin
        if type(target) == "number" then target = Origin end
        N = pt3d.normalize(camera-target)
        ph = pt3d.angle(vecK, N) 
        phi = ph*rad -- phi en degrés
        tet = pt3d.angle(vecI, pxy(N))
        theta = tet*rad -- theta en degrés
        d = pt3d.abs(camera-target)
    else
        camera = nil
        theta = theta or 30
        phi = phi or 60
        d = d or 15
        target = look or Origin
        d = math.abs(d)
        tet, ph = theta*deg, phi*deg
    end
    local cosTheta, sinTheta, cosPhi, sinPhi = cos(tet), sin(tet), cos(ph), sin(ph)
    if N == nil then N = M(cosTheta*sinPhi,sinTheta*sinPhi,cosPhi) end
    local mat = {Origin, M(-sinTheta,-cosPhi*cosTheta,cosTheta*sinPhi), M(cosTheta,-cosPhi*sinTheta,sinTheta*sinPhi), M(0,sinPhi,cosPhi) } -- mat.A(x,y,z) gives (a,b,c), and Z(a,b) is the affix of the projection of A on screen
    local invmat = invmatrix3d(mat) -- if Z(a,b) is the affix of the projection on screen, invmat.(a,b,0) gives A(x,y,z) on the screen so that the projection of A has the affix Z(a,b)
    if camera == nil then camera = target+d*N end -- camera
    local Zlookat = Z( cosTheta*target.y-sinTheta*target.x, -cosPhi*cosTheta*target.x-cosPhi*sinTheta*target.y+sinPhi*target.z)
    
    function graph3d:Isvisible(facet)
    -- facet est une liste de points 3d coplanaires
    -- la fonction renvoie true si la facette est visible (vecteur normal de même sens que n)
        local N = pt3d.prod(facet[2]-facet[1], facet[3]-facet[1])
        return self:Cosine_incidence(N,isobar3d(facet)) > 0 
    end    

    function graph3d:Proj3d(L) -- we redefine Proj3d
        local f = function(A)
            if isPoint3d(A) then
                local den = pt3d.dot(camera-A,N)
                if math.abs(den) > 1e-17 then
                    local k = d/den
                    local A1 = camera-k*(camera-A)
                    return Z( cosTheta*A1.y-sinTheta*A1.x, -cosPhi*cosTheta*A1.x-cosPhi*sinTheta*A1.y+sinPhi*A1.z)-Zlookat
                end
            else return A
            end
        end
        
        L = self:Mtransform3d(L) -- we apply the 3D matrix of the graph
        return ftransform3d(L,f) -- we return the projection on screen
    end 
    
    function graph3d:Proj3dV(L,A) -- we redefine Proj3dV, the origin for the vectors is the point A (target by defaut)
        A = A or target
        local B = self:Proj3d(A)
        local f = function(v)
            if isPoint3d(v) then
                return self:Proj3d(A+v)-B
            else return v
            end
        end
        return ftransform3d(L,f) -- we return the projection on screen
    end 
    
    function graph3d:Screenpos(z,d)
    -- renvoie les coordonnées spatiales d'un point ayant comme projeté sur l'écran le point d'affixe z,
    -- et se trouvant à une distance d (algébrique) du plan de l'écran
        z = toComplex(z)
        return applymatrix3d(M(z.re+Zlookat.re, z.im+Zlookat.im, pt3d.dot(target,N)), invmat)
    end
    
    function graph3d:Cosine_incidence(n,A)
    -- cosinus de l'angle d'incidence entre le vecteur n au point A et le vecteur dirigé vers l'observateur
        return pt3d.dot(pt3d.normalize(camera-A),n)
    end
    
    function graph3d:Observer_distance(A)
    -- l'abscisse de A sur l'axe issue de Origine, dirigé vers l'observateur
        return -pt3d.abs(camera-A)
    end
    
    function graph3d:circle3db(O,r,n)
        local U = pt3d.prod(n,self.Normal)
        if pt3d.abs(U) < 1e-8 then -- U est nul
            U = pt3d.prod(n,vecJ)
            if pt3d.abs(U) < 1e-8 then -- U est nul
                U = pt3d.prod(n,vecI)
            end
        end
        U = O+r*pt3d.normalize(U)
        return self:arc3db(U,O,U,r,1,n)
    end
    
    function graph3d:circle3db2(O,r,n)
        local U = pt3d.prod(n,self.Normal)
        if pt3d.abs(U) < 1e-8 then -- U est nul
            U = pt3d.prod(n,vecJ)
            if pt3d.abs(U) < 1e-8 then -- U est nul
                U = pt3d.prod(n,vecI)
            end
        end
        U = pt3d.normalize(U)
        local V = pt3d.normalize( pt3d.prod(n,U) )
        local a, b, c, d = table.unpack( self:Proj3d({O-r*V, O+r*V, O-r*U,O+r*U}) )
        -- a et b sont des points où les tangentes sont des lignes de fuite, parallèles à (cd)
        -- les projetés ont également des tangentes parallèles ce qui permet de déterminer le centre
        local o = (a+b)/2 -- centre de l'ellipse
        local u, v = cpx.normalize(d-c), a-o
        local mat = {o, cpx.abs(v)*u, v}
        local a1,b1,c1,d1 = table.unpack(mtransform({a,b,c,d}, invmatrix(mat)))
        -- dans le nouveau repère on a une ellipse de centre 0, passant par i (qui correspond à a1)
        -- et la droite (c1d1) est l'axe Ox et (a1b1) est l'axe Oy
        -- l'ellipse est l'image du cercle C(0,1) par une affinité d'axe Oy
        local alpha = d1.re/math.sqrt(1-d1.im^2)
        mat = multiplymatrix(mat,{0,alpha,cpx.I}) -- affinité d'axe Oy et de rapport alpha
        local L = circleb(0,1) -- cercle unité
        return map(function(z) if type(z) == "string" then return z else return self:Screenpos(z) end end, L) 
    end
    
    function graph3d:arc3db(B,A,C,r,sens,n)
        local n1, n2, V = pt3d.normalize(B-A), pt3d.normalize(C-A)
        if n == nil then V = pt3d.normalize(pt3d.prod(n1,n2)) else V = pt3d.normalize(n) end
        if pt3d.abs(V)<1e-12 then V = pt3d.normalize(n) end
        B = A+r*n1; C = A+r*n2
        local U = pt3d.prod(V, self.Normal)
        if pt3d.abs(U)<1e-12 then --plans parallèles
            local A1 = interDP( {camera,A-camera},{Origin,g.Normal})
            local alpha = pt3d.abs(A1-camera)/pt3d.abs(A-camera)
            --self:Darc(self:Proj3d(B), self:Proj3d(A), self:Proj3d(C),r*alpha,sens,draw_options)
            return {B,A,C,r*alpha,sens,"ca"}
        elseif math.abs( pt3d.dot(V,self.Normal) ) < 1e-3 then --plans perpendiculaires
            return {B,C,"l"} -- segment [C,B]
        else 
            local N2 = pt3d.normalize(pt3d.prod(U,V))
            local N1 = pt3d.normalize(U)
            local a1,a2,a3,a4,b,c,O,u,v
            a1,a2,a3,a4,b,c = table.unpack( self:Proj3d({A-r*N2, A+r*N2, A-r*N1, A+r*N1, B, C}) )
            O = (a1+a2)/2;  u = cpx.normalize(a4-a3); v = a1-O
            local mat = {O,cpx.abs(v)*u,v}            
            if cpx.det(mat[2],mat[3]) < 1e-3 then  
                return {B,C,"l"} -- segment [C,B]
            end
            local invm = invmatrix(mat)
            a4,b,c = table.unpack( mtransform({a4,b,c},invmatrix(mat)) ) 
            local y = a4.im 
            local x = a4.re
            local alpha = x/math.sqrt(1-y^2)
            local L = mtransform(ellipticarcb(b,0,c,alpha,1,sens), mat)
            return map(function(z) if type(z) == "string" then return z else return self:Screenpos(z) end end, L)
         end
    end
    
    function graph3d:Dcone(C,r,V,A,args) 
    -- ou Dcone(C,r,A,args)
    -- dessine un cône en fil de fer
    -- A est le sommet
    -- le centre de la face circulaire de rayon r orthogonale au vecteur V est C
    -- args est une table à 5 champs :
    -- {mode =0/1, hiddenstyle="dotted", hiddencolor = linecolor, edgecolor= linecolor, color="", opacity=1}
    -- mode = 0 mWireframe
    -- mode = 1 mGrid
    -- color = "" : pas de remplissage, color ~= "" remplissage avec gradient bi linéaire
        if isPoint3d(r) then -- ancien format : sommet, vecteur, rayon, args (cône droit)
        args = A; A = C
        r, V = V, r
        C = A+V; V = A-C
        elseif not isPoint3d(A) then -- format C,r,A,args (cône droit)
            args = A; A = V; V = A-C
        end
        args = args or {}
        local color = args.color
        color = color or ""
        local edgecolor = args.edgecolor or self.param.linecolor
        local hiddenstyle = args.hiddenstyle or Hiddenlinestyle
        local hiddencolor = args.hiddencolor or self.param.linecolor
        if not Hiddenlines then hiddenstyle = "noline" end        
        local mode = args.mode or mWireframe
        local opacity = args.opacity or 1
        local edgewidth = args.edgewidth or self.param.linewidth
        local angle = cpx.arg( self:Proj3d(A)-self:Proj3d(C))*rad
        if angle < 0 then angle = angle+180
        elseif angle > 180 then angle = angle-180 
        end
        local P = cone(C,r,V,A,50,true)
        local Pv, Ph = self:Classifyfacet(P)
        local BPv, BPh = border(Pv), border(Ph)
        
        local oldfillstyle = self.param.fillstyle
        local oldfillopacity = self.param.fillopacity
        local oldfillcolor = self.param.fillcolor
        local oldlinestyle = self.param.linestyle
        local oldlinecolor = self.param.linecolor
        local oldlinewidth = self.param.linewidth
        self:Lineoptions(nil,edgecolor,edgewidth)
        if mode == mGrid then self:Linestyle("noline") end
        -- hidden part
        if color ~= "" then 
            self:Filloptions("gradient", "left color="..color.."!25,right color = "..color.."!50,middle color="..color.."!18, shading angle="..strReal(angle),opacity)
        end
        if mode ~= mWireframe  then self:Dpolyline3d(BPh, true) end
        --visible part
        if color ~= "" then 
            self:Filloptions("gradient", "left color="..color.."!50,right color = "..color..",middle color="..color.."!10, shading angle="..strReal(angle),opacity)
        end
        self:Dpolyline3d(BPv, true)
        if color ~= "" then 
            self:Filloptions("gradient", "left color="..color.."!10,right color = "..color)
        end
        if self:Cosine_incidence(-V,C) > 0 then self:Dcircle3d(C,r,V) end
        -- hidden edges
        if (mode ~= mGrid) and (hiddenstyle ~= "noline") then -- partie cachée
            self:Filloptions("none"); self:Linestyle(hiddenstyle)
            self:Dpolyline3d(BPh, true)
        end
        if mode == mGrid then -- edges
            self:Linestyle(oldlinestyle)
            self:Dpoly(cone(C,r,V,A,35,true), {mode=mWireframe,hiddenstyle=hiddenstyle,hiddencolor=hiddencolor,edgecolor=edgecolor})
        end
        self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
        self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth); 
    end
    
    function graph3d:Dcylinder(C,r,V,A,args) 
    -- ou Dcylinder(C,r,A,args)
    -- dessine un cylindre en fil de fer
    -- C est le centre d'une face circulaire de rayon r orthogonale au vecteur V
    -- l'autre face a pour centre A 
    -- args est une table à 5 champs :
    -- {mode =0/1, hiddenstyle="dotted", hiddencolor = linecolor, edgecolor= linecolor, color="", opacity=1}
    -- mode = 0 mWireframe
    -- mode = 1 mGrid
    -- color = "" : pas de remplissage, color ~= "" remplissage avec gradient bi linéaire
        if isPoint3d(r) then -- ancien format : centre, vecteur, rayon, args (cylindre droit)
        args = A; 
        r, V = V, r
        A = C+V
        elseif not isPoint3d(A) then -- format C,r,A,args (cylindre droit)
            args = A; A = V; V = A-C
        end
        args = args or {}
        local color = args.color
        color = color or ""
        local edgecolor = args.edgecolor or self.param.linecolor
        local hiddenstyle = args.hiddenstyle or Hiddenlinestyle
        local hiddencolor = args.hiddencolor or self.param.linecolor
        if not Hiddenlines then hiddenstyle = "noline" end
        local mode = args.mode or mWireframe
        local opacity = args.opacity or 1
        local edgewidth = args.edgewidth or self.param.linewidth
        local angle = cpx.arg( self:Proj3d(A)-self:Proj3d(C))*rad
        if angle < 0 then angle = angle+180
        elseif angle > 180 then angle = angle-180 
        end
        local P = cylinder(C,r,V,A,50,true)
        local Pv, Ph = self:Classifyfacet(P)
        local BPv, BPh = border(Pv), border(Ph)
        local oldfillstyle = self.param.fillstyle
        local oldfillopacity = self.param.fillopacity
        local oldfillcolor = self.param.fillcolor
        local oldlinestyle = self.param.linestyle
        local oldlinecolor = self.param.linecolor
        local oldlinewidth = self.param.linewidth
        self:Lineoptions(nil,edgecolor,edgewidth)
        if mode == mGrid then self:Linestyle("noline") end
        -- hidden part
        if color ~= "" then 
            self:Filloptions("gradient", "left color="..color.."!25,right color = "..color.."!50,middle color="..color.."!18, shading angle="..strReal(angle),opacity)
        end
        if mode ~= mWireframe  then self:Dpolyline3d(BPh, true) end
        --visible part
        if color ~= "" then 
            self:Filloptions("gradient", "left color="..color.."!50,right color = "..color..",middle color="..color.."!10, shading angle="..strReal(angle),opacity)
        end
        self:Dpolyline3d(BPv,true)
        if color ~= "" then 
            self:Filloptions("gradient", "left color="..color.."!10,right color = "..color)
        end
        if self:Cosine_incidence(V,A) > 0 then self:Dcircle3d(A,r,V) end
        if self:Cosine_incidence(-V,C) > 0 then self:Dcircle3d(C,r,V) end
        -- hidden edges
        if (mode ~= mGrid) and (hiddenstyle ~= "noline") then -- partie cachée
            self:Filloptions("none"); self:Linestyle(hiddenstyle)
            self:Dpolyline3d(BPh, true)
        end
        if mode == mGrid then -- edges
            self:Linestyle(oldlinestyle)
            self:Dpoly(cylinder(C,r,V,A,35,false), {mode=mWireframe,hiddenstyle=hiddenstyle,hiddencolor=hiddencolor,edgecolor=edgecolor})
        end
        self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
        self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth); 
    end
     
    function graph3d:Dfrustum(A,R,r,V,B,args)
    -- dessine un tronc de cône en fil de fer
    -- A est le centre de la face de rayon R
    -- le centre de l'autre face  C=A+V et son rayon est r
    -- args est une table à 5 champs :
    -- {mode =0/1, hiddenstyle="dotted", hiddencolor = linecolor, edgecolor=linecolor, color="", opacity=1}
    -- mode = 0 fil de fer
    -- mode = 1 grille
    -- color = "" : pas de remplissage, color ~= "" remplissage avec linéaire
        if R == r then -- cylinder
            if not isPoint3d(B) then self:Dcylinder(A,V,R,B) -- B is args in this case
            else self:Dcylinder(A,R,V,B,args)
            end
            return
        end
        local C
        if isPoint3d(B) then -- slanted frustum
            C = dproj3d(B,{A,V})
            V = C-A
        else C = A+V; args = B; B = C
        end
        
        args = args or {}
        local color = args.color
        color = color or ""
        local edgecolor = args.edgecolor or self.param.linecolor
        local hiddenstyle = args.hiddenstyle or Hiddenlinestyle
        local hiddencolor = args.hiddencolor or self.param.linecolor
        if not Hiddenlines then hiddenstyle = "noline" end        
        local mode = args.mode or mWireframe
        local opacity = args.opacity or 1
        local edgewidth = args.edgewidth or self.param.linewidth
        local angle = cpx.arg( self:Proj3d(A)-self:Proj3d(B))*rad
        if angle < 0 then angle = angle+180
        elseif angle > 180 then angle = angle-180 
        end
        local P = frustum(A,R,r,V,B,50,true)
        local Pv, Ph = self:Classifyfacet(P)
        local BPv, BPh = border(Pv), border(Ph)
        
        local oldfillstyle = self.param.fillstyle
        local oldfillopacity = self.param.fillopacity
        local oldfillcolor = self.param.fillcolor
        local oldlinestyle = self.param.linestyle
        local oldlinecolor = self.param.linecolor
        local oldlinewidth = self.param.linewidth
        self:Lineoptions(nil,edgecolor,edgewidth)
        if mode == mGrid then self:Linestyle("noline") end
        -- hidden part
        if color ~= "" then 
            self:Filloptions("gradient", "left color="..color.."!25,right color = "..color.."!50,middle color="..color.."!18, shading angle="..strReal(angle),opacity)
        end
        if mode ~= mWireframe  then self:Dpolyline3d(BPh, true) end
        --visible part
        if color ~= "" then 
            self:Filloptions("gradient", "left color="..color.."!50,right color = "..color..",middle color="..color.."!10, shading angle="..strReal(angle),opacity)
        end
        self:Dpolyline3d(BPv, true)
       if color ~= "" then 
            self:Filloptions("gradient", "left color="..color.."!10,right color = "..color)
        end
        if self:Cosine_incidence(V,B) > 0 then self:Dcircle3d(B,r,V) end
        if self:Cosine_incidence(-V,A) > 0 then self:Dcircle3d(A,R,V) end
        -- hidden edges
        if (mode ~= mGrid) and (hiddenstyle ~= "noline") then -- partie cachée
            self:Filloptions("none"); self:Linestyle(hiddenstyle)
            self:Dpolyline3d(BPh, true)
        end
        if mode == mGrid then -- edges
            self:Linestyle(oldlinestyle)
            self:Dpoly(frustum(A,R,r,V,B,35,false), {mode=mWireframe,hiddenstyle=hiddenstyle,hiddencolor=hiddencolor,edgecolor=edgecolor})
        end
        self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
        self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth); 
    end
    
    function graph3d:Dsphere(A,r,args)
    -- dessine une sphère en fil de fer
    -- A est le sommet, r le rayon
    -- args est une table à 5 champs :
    -- {mode=0/1/2, hiddenstyle="dotted", hiddencolor = linecolor, edgecolor=linecolor,color="", opacity=1}
    -- color = "" : pas de remplissage, color ~= "" remplissage avec ball color
    -- si mode 1 : edgestyle = linestyle, edgecolor = linecolor, edgewidth = linewidth
    -- mode = 0 contour avec équateur
    -- mode = 1 contour avec méridiens et fuseaux
    -- mode = 2 contour seulement (cercle)
        args = args or {}
        args.color = args.color or ""
        args.edgecolor = args.edgecolor or self.param.linecolor
        args.hiddencolor = args.hiddencolor or args.edgecolor
        args.hiddenstyle = args.hiddenstyle or Hiddenlinestyle
        args.edgestyle = args.edgestyle or self.param.linestyle
        args.edgecolor = args.edgecolor or self.param.linecolor
        args.edgewidth = args.edgewidth or self.param.linewidth    
        args.mode = args.mode or 0
        args.opacity = args.opacity or 1
        
        local oldfillstyle = self.param.fillstyle
        local oldfillopacity = self.param.fillopacity
        local oldfillcolor = self.param.fillcolor
        local oldlinestyle = self.param.linestyle
        local oldlineopacity = self.param.lineopacity
        local oldlinecolor = self.param.linecolor
        local oldlinewidth = self.param.linewidth
        --self:Linecolor(args.edgecolor)
        local V = vecK
        if args.color ~= "" then
            self:Filloptions("gradient", "ball color="..args.color, args.opacity)
        else
            self:Filloptions("none")
        end
        local S = {A,r}
        local S1 = { (camera+S[1])/2, pt3d.abs(S[1]-camera)/2}
        local I,R,n = interSS(S,S1)
        local mat = invmatrix3d( self.matrix3d )
        local N = mLtransform3d(n,mat)
        self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth)
        self:Dcircle3d(I,R,N)
        if  args.mode == 0 then -- équateur
            local M1, M2 = interCS({A,r,V},S1)
            local M3 = rotate3d(M1,90,{A,vecK})
            local sens
            if self:Cosine_incidence(M3-A,M3) > 0 then sens = 1 else sens = -1 end
            self:Filloptions("none") --; self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth)
           self:Darc3d(M1,A,M2,r,sens,V)
            self:Lineoptions(args.hiddenstyle,args.hiddencolor)
            self:Darc3d(M1,A,M2,r,-sens,V)
        elseif args.mode == 1 then -- grille
            self:Dpoly(sphere(A,r),{mode=0,hiddenstyle=args.hiddenstyle,hiddencolor=args.hiddencolor,edgestyle=args.edgestyle,edgecolor=args.edgecolor,edgewidth=args.edgewidth})
        end
        self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
        self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth); 
        self:Lineopacity(oldlineopacity)
    end

    return {theta,phi,"central"}
end

--This function is automatically called at the next perspective change.
function close_central()
    circle3db = old_circle3db
    arc3db = old_arc3db
    graph3d.Dcone = old_Dcone
    graph3d.Dcylinder = old_Dcylinder
    graph3d.Dfrustum = old_Dfrustum
    graph3d.Dsphere = old_Dsphere
    graph3d.Proj3d = old_Proj3d
    graph3d.Proj3dV = old_Proj3dV
    graph3d.Cosine_incidence = old_Cosine_incidence
    graph3d.Isvisible = old_Isvisible
    graph3d.ScreenPos = old_Screenpos
    graph3d.Observer_distance = old_Observer_distance
end
