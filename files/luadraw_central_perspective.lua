-- luadraw_central_perspective.lua 
-- date 2026/09/05
-- version 3.5
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl

--functions to be redefined
local graph3d = luadraw.graph3d
local ld = luadraw
local cpx = ld.cpx
local Z = cpx.Z
local pt3d = ld.pt3d
local toPoint3d = pt3d.toPoint3d
local isPoint3d = pt3d.isPoint3d
local Origin, vecI, vecJ, vecK = pt3d.Origin, pt3d.vecI, pt3d.vecJ, pt3d.vecK
local M, Mc, Ms = pt3d.M, pt3d.Mc, pt3d.Ms
local map = ld.map
local notDef = ld.notDef

--local old_Dcone = graph3d.Dcone
--local old_Dcylinder = graph3d.Dcylinder
--local old_Dfrustum = graph3d.Dfrustum
--local old_Dsphere = graph3d.Dsphere
local old_Proj3d = graph3d.Proj3d
local old_Proj3dV = graph3d.Proj3dV
local old_Cosine_incidence = graph3d.Cosine_incidence
local old_Isvisible = graph3d.Isvisible
local old_Screenpos = graph3d.Screenpos
local old_Observer_distance = graph3d.Observer_distance

local camera = nil
local target = Origin

--This function is automatically called by the perspective function.
function luadraw.central_perspective(theta,phi,d,look) -- or central_perspective(camera, look)
    local N
    local cos, sin, tet, ph = math.cos, math.sin
    if isPoint3d(theta) then
        camera = theta
        target = phi or Origin
        if type(target) == "number" then target = Origin end
        N = pt3d.normalize(camera-target)
        ph = pt3d.angle(vecK, N) 
        phi = ph*ld.rad -- phi en degrés
        tet = pt3d.angle(vecI, ld.pxy(N))
        theta = tet*ld.rad -- theta en degrés
        d = pt3d.abs(camera-target)
    else
        camera = nil
        theta = theta or 30
        phi = phi or 60
        d = d or 15
        target = look or Origin
        d = math.abs(d)
        tet, ph = theta*ld.deg, phi*ld.deg
    end
    local cosTheta, sinTheta, cosPhi, sinPhi = cos(tet), sin(tet), cos(ph), sin(ph)
    if N == nil then N = M(cosTheta*sinPhi,sinTheta*sinPhi,cosPhi) end
    local mat = {Origin, M(-sinTheta,-cosPhi*cosTheta,cosTheta*sinPhi), M(cosTheta,-cosPhi*sinTheta,sinTheta*sinPhi), M(0,sinPhi,cosPhi) } -- mat.A(x,y,z) gives (a,b,c), and Z(a,b) is the affix of the projection of A on screen
    local invmat = ld.invmatrix3d(mat) -- if Z(a,b) is the affix of the projection on screen, invmat.(a,b,0) gives A(x,y,z) on the screen so that the projection of A has the affix Z(a,b)
    if camera == nil then camera = target+d*N end -- camera
    local Zlookat = Z( cosTheta*target.y-sinTheta*target.x, -cosPhi*cosTheta*target.x-cosPhi*sinTheta*target.y+sinPhi*target.z)
    
    
    function graph3d:Isvisible(facet)
    -- facet est une liste de points 3d coplanaires
    -- la fonction renvoie true si la facette est visible (vecteur normal de même sens que n)
        local N = pt3d.prod(facet[2]-facet[1], facet[3]-facet[1])
        return self:Cosine_incidence(N, pt3d.isobar3d(facet)) > 0 
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
        if self.scalexyz ~= 1 then
            L = ld.ftransform3d(L, self.scalexyz) -- we apply the scales on x, y and z axis
        end        
        return ld.ftransform3d(L,f) -- we return the projection on screen
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
        return ld.ftransform3d(L,f) -- we return the projection on screen
    end 
    
    function graph3d:Screenpos(z,d)
    -- renvoie les coordonnées spatiales d'un point ayant comme projeté sur l'écran le point d'affixe z,
    -- et se trouvant à une distance d (algébrique) du plan de l'écran
        z = cpx.toComplex(z)
        local rep = ld.applymatrix3d(M(z.re+Zlookat.re, z.im+Zlookat.im, pt3d.dot(target,N)), invmat)
        local m = self.matrix3d
        if not ld.isID3d(m) then
            rep = ld.mLtransform3d(rep, ld.invmatrix3d(m) )
        end
        return rep 
    end
    
    function graph3d:Cosine_incidence(n,A)
    -- cosinus de l'angle d'incidence entre le vecteur n au point A et le vecteur dirigé vers l'observateur
        return pt3d.dot(pt3d.normalize(camera-A),n)
    end
    
    function graph3d:Observer_distance(A)
    -- l'abscisse de A sur l'axe issue de Origine, dirigé vers l'observateur
        return -pt3d.abs(camera-A)
    end
    
    function graph3d:arc3db(B,A,C,r,sens,n)
        local n1, n2, V = pt3d.normalize(B-A), pt3d.normalize(C-A)
        if n == nil then V = pt3d.normalize(pt3d.prod(n1,n2)) else V = pt3d.normalize(n) end
        if pt3d.abs(V)<1e-12 then V = pt3d.normalize(n) end
        B = A+r*n1; C = A+r*n2
        local N = pt3d.normalize(ld.camera-A)
        local U = pt3d.prod(V,self.Normal)
        if (pt3d.abs(U)<1e-12) then --plans parallèles
            local A1 = ld.interDP( {A,A-camera},{target,self.Normal})
            local B1 = ld.interDP( {B,B-camera},{target,self.Normal})
            local r1 = pt3d.abs(B1-A1)
            local alpha = pt3d.abs(A1-camera)/pt3d.abs(A-camera)
            --self:Darc(self:Proj3d(B), self:Proj3d(A), self:Proj3d(C),r*alpha,sens,draw_options)
            return ld.arc3db(B,A,C,r1/alpha,sens,n)
        elseif math.abs( pt3d.dot(V,N) ) < 1e-12 then --plans perpendiculaires
            --print("ok")
            return ld.polyline2path3d( ld.arc3d(B,A,C,r,sens,n) )
        else 
            local pred = function(z)
                if type(z) == "string" then return z
                else
                    local P = self:Screenpos(z)
                    return ld.proj3dO(P, {A,V}, camera-P)
                end
            end
            local N2 = pt3d.normalize(pt3d.prod(U,V))
            local N1 = pt3d.normalize(U)
            local a1,a2,a3,a4,b,c,O,u,v
            local mat3d = self.matrix3d
            self:IDmatrix3d()
            a1,a2,a3,a4,b,c = table.unpack( self:Proj3d({A-r*N2, A+r*N2, A-r*N1, A+r*N1, B, C}) )
            O = (a1+a2)/2;  u = cpx.normalize(a4-a3); v = a1-O
            local mat = {O,cpx.abs(v)*u,v}            
            if math.abs(cpx.det(mat[2],mat[3])) < 1e-8 then 
                return ld.polyline2path3d( ld.arc3d(B,A,C,r,sens,n) )
            end
            local invm = ld.invmatrix(mat)
            a4,b,c = table.unpack( ld.mtransform({a4,b,c}, ld.invmatrix(mat)) ) 
            local y = a4.im 
            local x = a4.re
            local alpha = x/math.sqrt(1-y^2)
            local L = ld.mtransform( ld.ellipticarcb(b,0,c,alpha,1,sens), mat)
            self.matrix3d = mat3d
            return ld.ftransform(L, pred)
        end
    end
    
    function graph3d:circle3db(O,r,n)
        local U = pt3d.prod(n,self.Normal)
        if pt3d.abs(U) < 1e-12 then -- U est nul
            U = pt3d.prod(n,vecJ)
            if pt3d.abs(U) < 1e-12 then -- U est nul
                U = pt3d.prod(n,vecI)
            end
        end
        U = O+r*pt3d.normalize(U)
        return self:arc3db(U,O,U,r,1,n)
    end
    
    function graph3d:ellipse3db(center,r1,r2,dir1,normal)
    -- dir1 and normal must be two orthogonal vectors
        local A = center
        local U = pt3d.normalize(dir1)
        local N = pt3d.normalize(normal)
        local V = pt3d.normalize( pt3d.prod(N,U) )
        local mat = {Origin,vecI,r2/r1*vecJ,vecK}
        local P = {A,U,V,N}
        local invP = ld.invmatrix3d(P)
        local Q = ld.composematrix3d(P, ld.composematrix3d(mat,invP))
        local path = self:circle3db(A,r1,N)
        return ld.mtransform3d(path,Q)
    end   
    
    function graph3d:ellipticarc3db(B,A,C,r1,r2,sign,dir1,normal)
    -- dir1 and normal must be two orthogonal vectors
        normal = normal or pt3d.prod(B-A,C-A)
        local U = pt3d.normalize(dir1)
        local N = pt3d.normalize(normal)
        local V = pt3d.normalize( pt3d.prod(N,U) )
        local mat = {Origin,vecI,r2/r1*vecJ,vecK}
        local P = {A,U,V,N}
        local invP = ld.invmatrix3d(P)
        local Q = ld.composematrix3d(P, ld.composematrix3d(mat,invP))
        local path = self:arc3db(B,A,C,r1,sign,N)
        return ld.mtransform3d(path,Q)
    end   
    
    ld.camera = camera
    ld.target = target

    return {theta,phi,"central"}
end

--This function is automatically called at the next perspective change.
function luadraw.close_central()
    --graph3d.Dcone = old_Dcone
    --graph3d.Dcylinder = old_Dcylinder
    --graph3d.Dfrustum = old_Dfrustum
    --graph3d.Dsphere = old_Dsphere
    graph3d.Proj3d = old_Proj3d
    graph3d.Proj3dV = old_Proj3dV
    graph3d.Cosine_incidence = old_Cosine_incidence
    graph3d.Isvisible = old_Isvisible
    graph3d.ScreenPos = old_Screenpos
    graph3d.Observer_distance = old_Observer_distance
end
