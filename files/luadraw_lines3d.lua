-- luadraw_lines3d.lua (chargé par luadraw__graph3d)
-- date 2025/07/04
-- version 2.0
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.


function getbounds3d(L)
-- renvoie les limites xmin,xmax,ymin,ymax,zmin,zmax de la ligne polygonale 3d L
    if (L == nil) or (type(L) ~= "table") or (#L == 0) then return end
    if isPoint3d(L[1]) then L = {L} end -- liste de points 3d
    local A = L[1][1]
    local xmin, xmax, ymin, ymax, zmin, zmax = A.x, A.x, A.y, A.y, A.z, A.z
    for _,cp in ipairs(L) do
        for _,A in ipairs(cp) do
            if A.x < xmin then xmin = A.x end
            if A.x > xmax then xmax = A.x end
            if A.y < ymin then ymin = A.y end
            if A.y > ymax then ymax = A.y end
            if A.z < zmin then zmin = A.z end
            if A.z > zmax then zmax = A.z end
        end
    end
    return xmin, xmax, ymin, ymax, zmin, zmax
end

function merge3d(L)
-- L est une  liste de listes de points3d (ligne polygonale de l'espace), 
-- on recolle au mieux les composantes
    local S, num, aux, rep = {}, {}, {}, {}
    for _, cp in ipairs(L) do
        aux = {}
        for _,A in ipairs(cp) do
            table.insert(aux, insert3d(S,A,1e-10))
        end
        table.insert(num, aux)
    end
    for _,cp in ipairs(merge(num)) do
        aux = {}
        for _, k in ipairs(cp) do
            table.insert(aux,S[k])
        end
        table.insert(rep, aux)
    end
    return rep
end

-- calcul de courbes
function parametric3d(p,t1,t2,nbdots,discont,nbdiv) 
-- le paramétrage p est une fonction : t (réel) -> p(t) (pt3d)
    local saut = (discont or false)
    local niveau = (nbdiv or 5)
    local curve = {}
    local lastJump = true
    local dep = t1
    local fin = t2
    local nb = (nbdots or 50)
    local pas = (fin - dep) / (nb-1)
    local cp = {} -- composante connexe courante
    local t = dep
    local count = 0
    local seuil = math.abs(pas)

    local closeCp = function ()
            if count > 1 then
                table.insert(curve, cp)
            end
            lastJump = true
            count = 0
            cp ={}
        end
    
    local addCp = function (z)
            lastJump = false
            table.insert(cp, z)
            count = count + 1
        end
        
    local middle -- fonction récursive middle    
    middle = function(t1,t2,f1,f2,n)
        if (n > niveau) then 
            if saut then closeCp() end -- fermer la composante connexe
        else
           local tm = (t1 + t2) / 2  -- dichotomie
           local fm = p(tm)
           if (fm ~= nil) then
                if (f1 == nil) or (pt3d.N1(f1-fm) > seuil) then
                    middle(t1,tm,f1,fm,n+1)
                end
                addCp(fm)
                if (f2 == nil) or (pt3d.N1(f2-fm) > seuil) then
                    middle(tm,t2,fm,f2,n+1)
                end
            else -- fm = nil
                if (f1 ~= nil) then middle(t1,tm,f1,fm,n+1) else closeCp() end
                if (f2 ~= nil) then middle(tm,t2,fm,f2,n+1) else closeCp() end
            end
        end        
    end
    --corps de la fonction parametric
    local prec = nil -- point précédent le point courant
    local tPrec = nil -- valeur de t précédente
    local z = nil -- point courant
    for _ = 1, nb do
        A = p(t)
        if (A == nil) or notDef(A.x) or notDef(A.y) or notDef(A.z) then A = nil end
        if (A ~= nil) then
            if ( (prec ~= nil) and (pt3d.N1(prec-A) > seuil) ) or ( (prec == nil) and (t > dep) ) then
                middle(tPrec,t,prec,A,1)
            end
            addCp(A)
        else -- A = nil
            if (prec ~= nil) then middle(tPrec,t,prec,A,1) 
            else closeCp()
            end
        end
        tPrec = t
        prec = A
        t = t + pas
    end
    if (not lastJump) and (count > 1) then table.insert(curve, cp) end -- dernière composante
    if #curve > 0 then return curve end
end


 -- arc de cercle en courbe de Bézier
function arc3db(B,A,C,R,sens,normal)
-- calcule un arc de cercle de centre A, dans le plan ABC, de AB vers AC.
-- ce plan est orienté par le vecteur AB^AC ou le vecteur normal s'il est précisé
-- renvoie un chemin en courbes de Bézier avec des point3d
    local u, AC = B-A, C-A
    u = pt3d.normalize(u)
    local N = normal or pt3d.prod(u,AC)
    N = pt3d.normalize(N)
    local v = pt3d.prod(N,u)
    local a, b = pt3d.dot(AC,u), pt3d.dot(AC,v)
    local S = arcb(1,0,Z(a,b),R,sens) -- Z(a,b) est l'affixe de AC dans le repère (A,u,v)
    local chem = {}
    for _, P in ipairs(S) do
        if type(P) == "string" then table.insert(chem,P)
        else
            table.insert(chem, A+P.re*u+P.im*v)
        end
    end
    return chem -- renvoie un chemin en courbes de Bézier 3d
end

 -- arc de cercle en ligne polygonale
 function arc3d(B,A,C,r,sens,normal)
-- renvoie la liste de points de l'arc BAC (ligne polygonale 3d)
-- r = rayon
-- sens = 1/-1 (sens trigo ou inverse)
-- normal : vecteur normal au plan de l'arc
    if not isPoint3d(normal) then args = normal; normal = nil end
    local u, v = B-A, C-A
    u, v = pt3d.normalize(u), pt3d.normalize(v)
    local n = normal
    if n == nil then n = pt3d.prod(u,v) end
    if (n == nil) or pt3d.isNul(n) then return {{"nul"}} end
    n = pt3d.normalize(n)
    local V = r*u --vecteur de depart
    local W = pt3d.prod(n,V)
    local alpha = angle3d(u,v,1e-10)
    local fin
    if sens > 0 then fin = alpha
    else
        fin = 2*math.pi-alpha; W = -W
    end
    if alpha == 0 then fin = 2*math.pi end
    local L = parametric3d( function(t) return A+math.cos(t)*V+math.sin(t)*W end, 0,fin, math.max(2,math.floor(13*fin/math.pi)) )
    return L
end

-- cercle en courbe de Bézier
function circle3db(C,R,normal)
-- calcule un cercle de centre C de rayon R et normal au vecteur normal
-- renvoie un chemin en courbes de Bézier avec des point3d
    local N, u  = pt3d.normalize(normal)
    if N.x ~= 0 then u = M(-(N.y+N.z)/N.x,1,1)
    elseif N.y ~= 0 then u = M(1, -(N.x+N.z)/N.y,1)
    else u = M(1,1,-(N.y+N.x)/N.z)
    end
    u = pt3d.normalize(u); v = pt3d.prod(N,u)
    local S = circleb(0,R) -- dans le repère (C,u,v)
    local chem = {}
    for _, P in ipairs(S) do
        if type(P) == "string" then table.insert(chem,P)
        else
            table.insert(chem, C+P.re*u+P.im*v)
        end
    end
    return chem -- renvoie un chemin en courbes de Bézier
end

-- cercle en ligne polygonale
function circle3d(A,r,normal)
-- calcule un cercle de centre C de rayon R et normal au vecteur normal
-- renvoie une ligne polygonale
    local u = pt3d.prod(vecI,normal)
    if pt3d.isNul(u) then 
        u = pt3d.prod(vecJ,normal)
        if pt3d.isNul(u) then return end
    end
    return arc3d(A+u,A,A+u,r,1,normal, args )
end

---- intersections

function interDP(d,P) -- intersection droite/plan
-- intersection droite d={A,u} avec plan P={B,n} dans l'espace
    return proj3dO(d[1],P,d[2])
end

function interPP(P1,P2) -- intersection plan/plan
-- intersection plan P1={A1,n1} avec plan P2={A2,n2} dans l'espace
    local A, u = table.unpack(P1)
    local B, v = table.unpack(P2)
    local w = pt3d.prod(u,v)
    if pt3d.isNul(w) then return end -- plans parallèles
    local M = A + pt3d.dot(B-A,v) / pt3d.dot(w,w)*pt3d.prod(w,u)
    return {M,w}
end

function interDD(D1,D2,eps) -- intersection droite/droite
-- intersection droite D1={A1,u1} et D2={A2,u2}
    local A1, u1 = table.unpack(D1)
    local A2, u2 = table.unpack(D2)
    eps = eps or 1e-10
    local n = pt3d.prod(u1,u2)
    if  math.abs(pt3d.det(A2-A1, u1, u2)) <= eps then 
        return A1 + pt3d.det(A2-A1, u2, n)/pt3d.dot(n,n)*u1
    end
end


-- couper avec un plan (cut...) ou clipper avec un polyèdre convexe (clip...)

-- couper une ligne polygonale avec un plan
function cutpolyline3d(Lg,plane,close)
-- Lg est une liste de points 3d ou une liste de listes de points 3d  (ligne polygonale 3d)
-- plane est un plan {A,n}
-- la fonction coupe Lg avec le plan, la fonction renvoie :
-- la partie de L située devant le plan (du côté de n), la partie située derrière le plan et les points d'intersection
-- close indique si Lg doit être refermée
    if (type(Lg) ~= "table") or (#Lg == 0) then return end
    local Dev, Der = {}, {}
    local traiter = function(l)
        --l est un liste de points3d et de 'jump" que l'on splitte en composantes connexes
        local res = {} -- résultat
        local cp = {} -- composante courante
        local count = 0 -- nb d'éléments dans cp
        for _,A in ipairs(l) do
            if A ~= "jump" then 
                table.insert(cp,A)
                count = count + 1
            elseif count > 1 then 
                table.insert(res,cp); count = 0; cp = {}
            else count = 0; cp = {}
            end
        end
        if count > 1 then table.insert(res,cp) end
        return merge3d(res)
    end
    
    local A, n = table.unpack(plane)
    close = close or false
    local dev, der, inter = {}, {}, {}
    local AM, pscal, last, v
    local lastPos --0=out 1=on 2=in
    if isPoint3d(Lg[1]) then Lg = {Lg} end
    for _,L in ipairs(Lg) do
        dev, der, inter = {}, {}, {}
        last = nil
        if close then table.insert(L,L[1]) end
        for k,M in ipairs(L) do -- parcours par point
            AM = M-A; pscal = pt3d.dot(AM,n)
            if math.abs(pscal) < 1e-8 then pscal = 0 end -- M est dans le plan
            if pscal == 0 then table.insert(dev,M); table.insert(der,M); table.insert(inter,M); lastPos = "on"
            elseif pscal > 0 then -- M est du bon côté
                if lastPos == "out" then
                    v = last - M
                    res = proj3dO(last,{A,n},v)
                    if res ~= nil then 
                        table.insert(dev,res); table.insert(der,res); table.insert(der,"jump"); table.insert(inter,res) 
                    end
                elseif lastPos == "on" then table.insert(der,"jump")
                end
                lastPos = "in"
                table.insert(dev,M)
            else -- M est du mauvais côté
                if lastPos == "in" then
                    v = last - M
                    res = proj3dO(last,{A,n},v)
                    if res ~= nil then 
                        table.insert(dev,res); table.insert(dev,"jump");table.insert(der,res); table.insert(inter,res) 
                    end
                elseif lastPos == "on" then table.insert(dev,"jump")
                end
                lastPos = "out"
                table.insert(der,M)
            end
            last = M
        end
        if close then table.remove(L) end
        insert(Dev,traiter(dev)); insert(Der, traiter(der))
    end
    return Dev, Der, inter
end

--clipper une ligne polygonale avec un polyèdre convexe
function clippolyline3d(L, poly, exterior, close)
-- clippe la ligne polygonale 3d L avec le polyèdre convexe poly (polyèdre)
-- exterior (true/false) indique si on conserve l'extérieur ou pas
-- close indique si L doit être refermée
    exterior = exterior or false
    close = close or false
    local A, B, C, u, L1
    if not exterior then -- on conserve l'intérieur
        for _,facet in ipairs(poly.facets) do
            A = poly.vertices[facet[1]]
            B = poly.vertices[facet[2]]
            C = poly.vertices[facet[3]]
            u = pt3d.prod(B-A,C-A)
            if u ~= nil then
                L = cutpolyline3d(L,{A,-u},close)
            end
        end
        return L
    else -- on conserve l'extérieur
        local rep = {}
        for _,facet in ipairs(poly.facets) do
            A = poly.vertices[facet[1]]
            B = poly.vertices[facet[2]]
            C = poly.vertices[facet[3]]
            u = pt3d.prod(B-A,C-A)
            if u ~= nil then
                L1, L = cutpolyline3d(L,{A,u},close)
                insert(rep,L1)
            end
        end
        return rep
    end
end    

--clipper une droite avec un polyèdre convexe
function clipline3d(line, poly)
-- clippe la droite line avec le polyèdre convexe poly (polyèdre)
-- on renvoie la partie intérieure au polyèdre uniquement
    local A, u = table.unpack(line)
    u = pt3d.normalize(u)
    if u == nil then return end
    local x1,x2,y1,y2,z1,z2 = getbounds3d(poly.vertices)
    local O = M(x1+x2,y1+y2,z1+z2)/2 -- centre de la boite contenant le polyèdre
    local delta = (math.abs(x2-x1)+math.abs(y2-y1)+math.abs(z2-z1))/2
    local O1 = proj3d(O,line)
    local d = pt3d.N1(O-O1)
    local t = 1+delta+d
    local A, B = O1-t*u, O1+t*u -- deux points de la droite mais hors de la boite
    local L = clippolyline3d({A,B},poly)
    return L
end

function bezier3d(a,c1,c2,b,nbdots)
--renvoie les points de la courbe de Bézier allant de a à b ayant comme points de contrôle c1 et c2
-- a,c1,c2,b sont des points 3d.
    if (a == nil) or (b == nil) or (c1 == nil) or (c2 == nil) then return end
    local u, v, w = b-3*c2+3*c1-a, 3*a-6*c1+3*c2, 3*c1-3*a
    local p = function(t)
            return a+t*(w+t*(v+t*u))
        end
    return parametric3d(p,0,1,nbdots or 8)
end


function path3d(chemin)
-- renvoie les points constituant le chemin
-- celui-ci est une table de points, valeurs et d'instructions ex: {-1,2+i,3,"l", 4, "m", -2*i,-3-3*i,"l","cl",...}
-- "m" pour moveto, "l" pour lineto, "b" pour bézier, "c" pour cercle, "ca" pour arc de cercle, "cl" pour close
    if (chemin == nil) or (type(chemin) ~= "table") or (#chemin < 3) then return end
    local res = {} -- résultat
    local crt = {} -- composante courante
    local aux = {} -- lecture en cours
    local last, first = nil, nil -- dernier lu et premier à venir
    
    local lineto = function() -- traitement du lineto
    -- on met les points dans la composante courante et on vide aux
            for _,z in ipairs(aux) do
                table.insert(crt,z)
            end
            first = last
            aux = {}
    end
    
    local moveto = function() -- traitement du moveto
    -- on démarre une nouvelle composante
            if #crt > 0 then table.insert(res,crt); crt = {} end -- on démarre une nouvelle composante avec le move
            first = nil
            aux = {last}
    end
    
    local close = function() -- traitement du closepath
        -- en principe il y a eu une instruction avant autre que move, aux doit être vide et pas crt
        if (#crt == 0) then return end
        table.insert(crt,crt[1]) -- on ferme la composante courante en ajoutant le premier point
        first = crt[1]
        aux = {}
    end
    
    local Bezier = function()
        -- aux contient une courbe de bézier
        if first ~= nil then 
            table.insert(aux,1,first); table.remove(crt)
        end
        local C = bezier3d(table.unpack(aux)) -- renvoie une liste de liste de complexes
        for _, z in ipairs(C[1]) do
            table.insert(crt,z)
        end
        first = last
        aux = {}
    end
    
    local Circle = function()
    -- il faut aux = {a,c,v} (un point, le centre et un vecteur normal)
        if first ~= nil then 
            table.insert(aux,1,first); table.remove(crt)
        end
        local a, c, v = table.unpack(aux)
        local C = circle3d(c,pt3d.abs(c-a),v)
        if C ~= nil then
            for _, z in ipairs(C[1]) do
                table.insert(crt,z)
            end
            first = crt[#crt]
        end
        aux = {}
    end
    
    local Arc = function()
        -- il faut aux = {b,a,c,r,sens,v}
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        local C = arc3d(table.unpack(aux))
        if C ~= nil then
            for _, z in ipairs(C[1]) do
                table.insert(crt,z)
            end
            first = crt[#crt]
        end
        aux = {}
    end
    
    local traiter = { ["s"]=Spline, ["l"]=lineto, ["m"]=moveto, ["cl"]=close, ["b"]=Bezier, ["c"]=Circle, ["ca"]=Arc} 
    for _, z in ipairs(chemin) do
        if (type(z) == "number") or isPoint3d(z) then table.insert(aux,z); last = z 
        else
            if type(z) == "string" then traiter[z]() end
        end
    end
    if #crt > 0 then table.insert(res, crt) end    
    if #res > 0 then return  res end
end
