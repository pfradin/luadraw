-- luadraw_lines.lua (chargé par luadraw__calc)
-- date 2025/02/21
-- version 1.0
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.


-- utilitaires sur les liste de complexes (composantes connexes)
function len(L)
-- renvoie la longueur de L  qui doit être une liste de complexes
    local long = 0
    local b,a = L[1]
    for k = 2, #L do
        a = b ; b = L[k]
        long = long + cpx.abs(b-a)
    end
    return long
end

function getdot(x,L) -- abscisse  dans [0;1], L composante connexe
-- renvoie le point d'abscisse curviligne demandée le long de la ligne polygonale (0=premier, 1=dernier).}
    local d = x*len(L)
    if x == 0 then return L[1] 
    else
        if x == 1 then return L[#L] 
        else
            local long = 0
            local b, a =L[1]
            for k = 2, #L do
                a = b; b = L[k]
                long = long + cpx.abs(b-a) 
                if long >= d then  
                    local u = b-a 
                    return b + (d-long)*u/cpx.abs(u)
                end
            end
        end
    end
end

function getbounds(L)
-- renvoie les limites xmin,xmax,ymin,ymax de la ligne polygone L
    if (L == nil) or (type(L) ~= "table") or (#L == 0) then return end
    if (type(L[1]) == "number") or isComplex(L[1]) then L = {L} end -- liste de réels/complexes
    local z = L[1][1]
    z = toComplex(z)
    local xmin, xmax, ymin, ymax = z.re, z.re, z.im, z.im
    for _,cp in ipairs(L) do
        for _,z in ipairs(cp) do
            z = toComplex(z)
            if z.re < xmin then xmin = z.re end
            if z.re > xmax then xmax = z.re end
            if z.im < ymin then ymin = z.im end
            if z.im > ymax then ymax = z.im end
        end
    end
    return xmin, xmax, ymin, ymax
end

function cut(L,A,before)
-- renvoie la ligne polygonale L coupée au point A
-- la partie qui suit A est renvoyée si before vaut true (coupure avant A), sinon c'est la partie située avant.
    if (L == nil) or (type(L) ~= "table") or (#L == 0) or (A == nil) then return end
    A = toComplex(A)
    if A == nil then return end
    if (type(L[1]) == "number") or isComplex(L[1]) then L = {L} end -- liste de réels/complexes
    before = before or false -- coupure après A
    local coupee, avant, apres = false, {}, {}
    
    local cutcp = function(cp) -- coupure composante connexe
        if #cp < 2 then return false end
        local B, C, ok = nil, cp[1], false
        local av, ap = {},{}
        for k = 2, #cp do
            B = C; C = cp[k]
            table.insert(av,B)
            -- on teste si A est dans le segment [B,C]
            local z = cpx.bar(A-B)*(C-A)
            if (z.re >= 0) and isNul(z.im) then -- c'est bon
                if A ~= B then table.insert(av,A) end
                if A ~= C then table.insert(ap,A) end
                for j = k, #cp do 
                    table.insert(ap,cp[j])
                end
                table.insert(avant, av); table.insert(apres, ap)
                return true
            end
        end
        return false
    end
    
    for _, cp in ipairs(L) do
        if coupee then -- coupure déjà faite
            table.insert(apres, cp)
        else
            coupee = cutcp(cp)
            if not coupee then table.insert(avant,cp) end
        end
    end
    if before then return apres else return avant end
end

function sequence(f, u0, n)
-- représentation graphique (escalier ou colimaçon) de la suite définie par $u_{n+1}=f(u_n)$.}
-- la fonction renvoie une liste de points
    if (f == nil) or (u0 == nil) or (n == nil) then return end
    local x, L = u0, {Z(u0,0)}
    for k = 1, n do
        y = f(x)
        if y == nil then return L end
        table.insert(L,Z(x,y)); table.insert(L,Z(y,y))
        x = y
    end
    return L
end

---- arc de cercle
function arc(B,A,C,r,sens) 
-- renvoie la liste des points de l'arc de cercle de centre A, allant de B à C avec r le rayon en cm, et sens qui vaut +/-1 ( 1 = sens trigo).
    A, B, C = toComplex(A), toComplex(B), toComplex(C)
    if (A == nil) or (B == nil) or (C == nil) or (r == nil) or (type(r) ~= "number") or (r <= 0) then return end
    local u, v = B-A, C-A
    if (u == nil) or (v == nil) then return end
    local deb = cpx.arg(u) -- angle (Ox,u) entre -pi et pi   
    local fin = cpx.arg(v) 
    if (deb == nil) or (fin == nil) then return end
    if sens == 1 then
        while deb >= fin do deb = deb-2*math.pi end
    else
        while deb <= fin do fin = fin-2*math.pi end
    end
    local p = function(t)
            return A + r*cpx.exp(cpx.I*t)
        end
    local nbdots = math.ceil(math.abs(fin-deb)*12/math.pi)
    return parametric(p,deb,fin,nbdots)
end


function arcb(b,a,c,r,sens) --renvoie un arc de cercle sous forme de chemin avec des courbes de Bézier
-- arc de cercle, la fonction renvoie un chemin
    a, b, c = toComplex(a), toComplex(b), toComplex(c)
    if (a == nil) or (b == nil) or (c == nil) or (r == nil) or (type(r) ~= "number") or (r <= 0) then return end
    local u,v = r*(b-a)/cpx.abs(b-a)
    local n = cpx.I*u
    if sens < 0 then n = -n end
    angle = cpx.arg((c-a)/(b-a))
    if (sens > 0) and (angle <= 0) then angle = angle+2*math.pi
    else
        if sens < 0 then
            if angle >= 0 then angle = 2*math.pi - angle else angle = math.abs(angle) end
        end
    end
    local du, dn = u*0.555, n*0.555
    local chemin = {a+u}
    while angle >= math.pi/2 do
        insert(chemin,{a+u+dn,a+du+n,a+n,"b"})
        angle = angle - math.pi/2
        v = u; dv = du; u = n; du = dn; n = -v; dn = -dv
    end
    local d, ca, sa = a+r*(c-a)/cpx.abs(c-a), math.cos(angle/2), math.sin(angle/2)
    local k = 4*(1-ca)*ca/(3*sa^2)*1.005
    local e = interD({a+u,n},{d,cpx.I*(d-a)})
    if e ~= nil then
        insert(chemin, {a+u+k*(e-a-u), d+k*(e-d), d,"b"})
    end
    return chemin
end

function angleD(B,A,C,r) -- appelée par la méthode Dangle
-- A,B,C trois complexes et r une distance (cm)
-- renvoie 3 points pour dessiner l'angle (AB,AC) sous forme de parallélogramme
    local u, v = B-A, C-A
    r = (r or 0.25)
    u = r*u/cpx.abs(u)
    v = r*v/cpx.abs(v)
    if (u == nil) or (v == nil) then return end
    return A+u,A+u+v,A+v
end

function seg(a,b,scale) -- appelée par la méthode Dseg
-- scale permet de jouer sur la longueur du segment
    a = toComplex(a)
    b = toComplex(b)
    scale = scale or 1
    if scale ~= 1 then
        local u = b-a
        local l = cpx.abs(u)
        local d = (scale-1)*l/2
        u = d*u/cpx.abs(u)
        return {a-u,b+u}
    else
        return {a,b} -- une composante à 2 points
    end

end

function square(a, b, sens) -- appelée par la méthode Dsquare
-- renvoie une ligne polygonale représentant un carré de sommets consécutifs les complexes a et b 
-- sens = 1 (sens trigo) ou -1 
    a = toComplex(a)
    b = toComplex(b)
    if (a == nil) or (b == nil) then return end
    local u = cpx.I*(b-a) -- rotation de pi/2 du vecteur z2-z1
    if sens < 0 then u = -u end
    return {a,b,b+u,a+u}
end 

function rectangle(a,b,c)
-- renvoie le rectangle ayant comme sommets  consécutifs a et b (complexe) tel que le côté opposé passe par c.
    a = toComplex(a)
    b = toComplex(b)
    c = toComplex(c)
    if (a == nil) or (b == nil) or (c == nil) or (a == b) then return end
    local u = b-a
    local a1, b1 = proj(a,{c,u}), proj(b,{c,u})
    if (a1 == nil) or (b1 == nil) then return end
    return {a,b,b1,a1}
end

function ellipse(c,rx,ry,inclin)
-- renvoie les points de l'ellipse de centre c et de rayons rx et ry
    inclin = (inclin or 0) -- inclinaison en degrés par rapport à l'horizontale
    c = toComplex(c)
    if (c == nil) or (rx == nil) or (ry == nil) or (type(rx) ~= "number") 
     or (type(ry) ~= "number") or (rx <= 0) or (ry <= 0) then return end
    local f = 1
    if inclin ~= 0 then
        f = cpx.exp(cpx.I*inclin*math.pi/180)
    end
    local p = function(t)
            return c + f*Z(rx*math.cos(t),ry*math.sin(t))
        end
    return parametric(p,-math.pi,math.pi)
end

function ellipseb(c,rx,ry,inclin)
-- renvoie l'ellipse de centre c et de rayons rx et ry sous forme de chemin avec des courbes de Bézier
    inclin = (inclin or 0) -- inclinaison en degrés par rapport à l'horizontale
    c = toComplex(c)
    if (c == nil) or (rx == nil) or (ry == nil) or (type(rx) ~= "number") 
     or (type(ry) ~= "number") or (rx <= 0) or (ry <= 0) then return end
    local f = 1
    if inclin ~= 0 then
        f = cpx.exp(cpx.I*inclin*math.pi/180)
    end
    local u, n = Z(rx,0)*f, Z(ry,0)*cpx.I*f
    local du, dn = u*0.555, n*0.555
    return {c+u, c+u+dn, c+du+n, c+n, "b", 
            c-du+n, c-u+dn, c-u, "b", 
            c-u-dn, c-du-n, c-n, "b", 
            c-n+du, c-dn+u, c+u, "b"}
end

function circle(c,r,d)  -- circle(center,radius) ou circle(a,b,c) (3 points)
-- renvoie les points du cercle de centre c et de rayon r ou passant par les trois points c,r et d
    c = toComplex(c)
    if (c == nil) or (r == nil) then return end
    if (d == nil) then
        if (type(r) ~= "number") or (r <= 0) then return end
    else -- cercle passant par c, r et d
        r, d = toComplex(r), toComplex(d)
        if (r == nil) or (d == nil) then return end
        c = interD(med(c,r), med(c,d))
        if c == nil then return end
        r = cpx.abs(c-r)
    end
    
    local p = function(t)
        return c + r*cpx.exp(cpx.I*t)
    end
    
    return parametric(p,-math.pi,math.pi)
end

function circleb(c,r,d)  -- circleb(center,radius) ou circleb(a,b,c) (3 points)
-- renvoie le cercle de centre c et de rayon r ou passant par les trois points c,r et d sous forme de chemin avec des courbes de Bézier
    c = toComplex(c)
    if (c == nil) or (r == nil) then return end
    if (d == nil) then
        if (type(r) ~= "number") or (r <= 0) then return end
    else -- cercle passant par c, r et d
        r, d = toComplex(r), toComplex(d)
        if (r == nil) or (d == nil) then return end
        c = interD(med(c,r), med(c,d))
        if c == nil then return end
        r = cpx.abs(c-r)
    end
    local u = Z(r,0)
    local n = cpx.I*u
    local du, dn  = u*0.555, n*0.555
    return {c+u, c+u+dn, c+du+n, c+n, "b", 
            c-du+n, c-u+dn, c-u, "b", 
            c-u-dn, c-du-n, c-n, "b",
            c-n+du, c-dn+u, c+u, "b"}
end

function ellipticarc(b, a, c, rx, ry, sens, inclin)
-- renvoie la liste des points constituant un arc d'ellipse de AB vers AC
-- rx et ry sont en cm
-- sens = +/-1 (1 pour le sens trigo), inclin est l'inclinaison en degrés par rapport à l'horizontale
    inclin = (inclin or 0) -- inclinaison en degrés par rapport à l'horizontale
    sens = (sens or 1)
    a, b, c = toComplex(a), toComplex(b), toComplex(c)
    if (a == nil) or (b == nil) or (c == nil) or (rx == nil) or (ry == nil) or (type(rx) ~= "number") 
     or (type(ry) ~= "number") or (rx <= 0) or (ry <= 0) then return end
    local theta = inclin*math.pi/180
    local f = 1
    if inclin ~= 0 then
        f = cpx.exp(cpx.I*theta)
        b = a + (b-a)/f
        c = a + (c-a)/f
    end
    local angleb, anglec = cpx.arg(b-a), cpx.arg(c-a) -- angles entre -pi et pi
    local t1 = math.atan(rx*math.tan(angleb)/ry)
    local t2 = math.atan(rx*math.tan(anglec)/ry)
    if angleb < -math.pi/2 then t1 = t1-math.pi end
    if angleb > math.pi/2 then t1 = t1+math.pi end
    if anglec < -math.pi/2 then t2 = t2-math.pi end
    if anglec > math.pi/2 then t2 = t2+math.pi end
    if sens == 1 then
        while t1 >= t2 do t1 = t1-2*math.pi end
    else
        while t1 <= t2 do t1 = t1 + 2*math.pi end
    end
    local p = function(t)
            return a + f*Z(rx*math.cos(t),ry*math.sin(t))
        end
    local nbdots = math.ceil(math.abs(t2-t1)*12/math.pi)
    return parametric(p,t1,t2)
end

function ellipticarcb(b, a, c, rx, ry, sens, inclin)
-- renvoie l'arc d'ellipse de AB vers AC sous forme de chemin avec des courbes de Bézier
-- rx et ry sont en cm
-- sens = +/-1 (1 pour le sens trigo), inclin est l'inclinaison en degrés par rapport à l'horizontale
    inclin = (inclin or 0) -- inclinaison en degrés par rapport à l'horizontale
    sens = (sens or 1)
    a, b, c = toComplex(a), toComplex(b), toComplex(c)
    if (a == nil) or (b == nil) or (c == nil) or (rx == nil) or (ry == nil) or (type(rx) ~= "number") 
     or (type(ry) ~= "number") or (rx <= 0) or (ry <= 0) then return end
    local theta = inclin*math.pi/180
    local t = ry/rx
    local v = 1
    if inclin ~= 0 then
        v = cpx.exp(cpx.I*theta)
    end
    local u = cpx.I*v
    local mat1 = matrixof(function(z) return affin(z, {a,v}, u, 1/t) end)
    local mat2 = matrixof(function(z) return affin(z, {a,v}, u, t) end)
    local b1, c1 = table.unpack(mtransform({b,c},mat1))
    local C = arcb(b1,a,c1,rx,sens)
    return mtransform(C,mat2)
end 


function polyreg(centre,sommet,nbcotes,sens) -- ou polyreg(sommet1,sommet2,nbcotes, sens) appelée par la méthode Dpolyreg
-- renvoie une ligne polygonale représentant un polygone régulier
    if sens == nil then
        centre = toComplex(centre)
        sommet = toComplex(sommet)
        if (centre == nil) or (sommet == nil) then return end    
        sommet = sommet - centre
    else
        s1 = toComplex(centre) ; s2 = toComplex(sommet)
        if (s1 == nil) or (s2 == nil) then return end
        local u = s2-s1
        local d = cpx.abs(u)
        local v = cpx.I*u/d
        local h = d/2/math.tan(math.pi/nbcotes)
        if sens > 0 then
            centre = (s1+s2)/2 + h*v 
        else centre = (s1+s2)/2 - h*v
        end
        sommet = s1 - centre
    end
    local res = {}
    local itheta = cpx.I*2*math.pi/nbcotes
    for k = 1, nbcotes do  
        table.insert(res, centre + cpx.exp(itheta*k)*sommet )
    end
    return res
end 

-- tangentes
function tangent(p,t0,long)
-- tangente à la courbe paramétrée par t -> p(t) (à valeurs complexes)
-- au point de paramètre t0
-- si long vaut nil on renvoie une droite, sinon un segment
    if (p == nil) or (t0 == nil) then return end
    local A = p(t0)
    if (A == nil) then return end
    local v = (p(t0+1E-6)-p(t0))*1E6 -- vecteur tangent
    if (v == nil) or cpx.isNul(v) then return end
    if long == nil then return {A,v} -- on renvoie une droite
    else 
        local u = long*v/cpx.abs(v)/2
        return {A-u,A+u} -- on renvoie un segment
    end
end

function tangentC(f,x0,long)
-- tangente à la courbe cartésienne d'équation y=f(x)
-- au point d'abscisse x0
-- si long vaut nil on renvoie une droite, sinon un segment
    if (f == nil) or (x0 == nil) then return end
    local y0 = f(x0)
    local A = Z(x0,y0)
    if (A == nil) then return end
    local v = Z(1, (f(x0+1E-6)-y0)*1E6) -- vecteur tangent
    if (v == nil) or cpx.isNul(v) then return end
    if long == nil then return {A,v} -- on renvoie une droite
    else 
        local u = long*v/cpx.abs(v)/2
        return {A-u,A+u} -- on renvoie un segment
    end
end
    
--  intersection entre 2 lignes polygonales (liste de composantes connexes)
function interL(L1, L2)
-- calcule et renvoie la liste des points d'intersection de deux lignes polygonales L1 et L2 (listes de complexes ou liste de listes de complexes)
    local firstA
    local firstC

    local interSegSeg = function(A,u,C,v)
    -- calcule et renvoie le point d'intersection du segment [A,A+u] avec le segment [C,C+v], ou nil
    -- A, B, u et v sont supposés être 4 complexes avec u et v non nuls
        C = toComplex(C)
        v = toComplex(v)
        -- if (C == nil) or (v == nil) then return end
        local E = interD({A,u}, {C,v})
        if E ~= nil then
            local t1 = cpx.dot(E-A,u) / cpx.abs(u)^2
            local t2 = cpx.dot(E-C,v) / cpx.abs(v)^2
            if (t1 ~= nil) and ( (firstA and (0 <= t1)) or ( (not firstA) and (0 < t1) ) ) and (t1 <= 1) and
            (t2 ~= nil) and ( (firstC and (0 <= t2)) or ( (not firstC) and (0 < t2) ) ) and (t2 <= 1) then
                return E
            end
        end
    end
    
    local interSegL = function(A,B,L)
    -- calcule et renvoie la liste des points d'intersection entre le segment [A,B] et la composante connexe L (liste de complexes)
    -- A et B sont supposés être des complexes, et L une liste de complexes d'au moins 2 nombres.
        A = toComplex(A) ; B = toComplex(B)
        -- if (A == nil) or (B == nil) then return end
        local res = {}
        local D = L[1]
        firstC = true
        for k = 2, #L do
            local C = D
            D = L[k]
            if D ~= C then
                local E = interSegSeg(A,B-A,C,D-C)
                if E ~= nil then table.insert(res,E) end
            end
            firstC = false
        end
        if #res > 0 then return res end
    end
    
    local interLL = function(L1,L2)
    -- calcule et renvoie la liste des points d'intersection deux composantes connexes L1 et L2 (liste de complexes)
    -- L1 et L2 sont supposés être deux liste de complexes d'au moins 2 nombres.
        if (L1 == nil) or (type(L1) ~= "table") or (#L1 <= 1) or (L2 == nil) or (type(L2) ~= "table") or (#L2 <= 1)
        then return end
        local res = {}
        local B = L1[1]
        firstA = true
        for k = 2, #L1 do
            local A = B
            B = L1[k]
            local aux = interSegL(A,B,L2)
            if aux ~= nil then
                for _, E in ipairs(aux) do
                    table.insert(res, E)
                end
            end
            firstA =false
        end
        if #res > 0 then return res end
    end
    -- corps de la fonction interL
    if (L1 == nil) or (type(L1) ~= "table") or (#L1 == 0) or (L2 == nil) or (type(L2) ~= "table") or (#L2 == 0)
    then return end
    if (type(L1[1]) == "number") or isComplex(L1[1]) then L1 = {L1} end -- liste de réels/complexes
    if (type(L2[1]) == "number") or isComplex(L2[1]) then L2 = {L2} end -- liste de réels/complexes    
    local res= {}
    for _, cp1 in ipairs(L1) do
        for _, cp2 in ipairs(L2) do
            local aux = interLL(cp1, cp2)
            if aux ~= nil then
                for _, E in ipairs(aux) do
                    table.insert(res,E)
                end
            end
        end
    end
    if #res > 0 then return res end
end-- of interL

-- intersection de droites
function interD(d1, d2)
--renvoie le point d'intersection entre les droites d1 = {A,u1} et d2 = {B,u2}
    if (d1 == nil) or (type(d1) ~= "table") or (#d1 ~= 2) then return end
    if (d2 == nil) or (type(d2) ~= "table") or (#d2 ~= 2) then return end
    local A, u = table.unpack(d1)
    local B, v = table.unpack(d2)
    return projO(A,d2,u)
end

-- intersection de 2 chemins (path)
function interP(P1,P2)
    local L1, L2 = path(P1), path(P2) -- transformation en liste de points
    return interL(L1,L2)
end

-- intersection entre une droite et une ligne polygonale
function interDL(d,L)
--renvoie les points d'intersection entre la droite d={A,u} et la ligne polygonale L
    if (d == nil) or (type(d) ~= "table") or (#d ~= 2) then return end
    local xmin, xmax, ymin, ymax = getbounds(L)
    return interL( clipline(d,xmin,xmax,ymin,ymax), L)
end


-- fonction de clipping avec une fenêtre

function needclip(L,xmin,xmax,ymin,ymax)
-- renvoie true ou false suivant que L a besoin d'être clippée
-- L est une liste de complexes ou une liste de listes de complexes

   local isOut = function(z)
    return  (z.re < xmin) or
            (z.im < ymin) or
            (z.re > xmax) or
            (z.im > ymax)
    end
    
    local test = function(cp)
        for _, z in ipairs(cp) do
            z = toComplex(z)
            if (z ~= nil) and isOut(z) then return true end
        end
        return false
    end
    
    if (type(L) ~= "table") or (#L == 0) then return end
    if (type(L[1]) == "number") or isComplex(L[1]) then L = {L} end
    for _,cp in ipairs(L) do
        if test(cp) then return true end
    end
    return false
end

function clipseg(A,B,xmin,xmax,ymin,ymax)
-- clippe le seg [A,B] avec la fenêtre [xmin, xmax]x[ymin, ymax]
-- A et B sont deux points (complexes)
-- la fonction renvoie un segment {c,d} ou nil
    local contains = function(T,a)
        for _,x in ipairs(T) do
            if x == a then return true end
        end
        return false
    end
    
    A = toComplex(A); B = toComplex(B)
    local U = B-A
    if (U.re == 0) and (U.im == 0) then return end
    local res = {}
    local C = { Z(xmin, ymin),
                Z(xmax, ymin),
                Z(xmax, ymax),
                Z(xmin, ymax),
                Z(xmin, ymin) }
    local test1 = cpx.det(A-C[1],U)
    for i = 2, 5 do
        local test2 = cpx.det(A-C[i],U)
        if test1*test2 <= 0 then -- C[i] et C[i-1] sont de part et d'autre de (AB)
            test1 = test2
            local V = C[i]-C[i-1]
            local t = (cpx.det(C[i]-A,V) / cpx.det(U,V))
                if (not notDef(t)) and (not contains(res,t)) then table.insert(res, t) end
        end
    end
    if #res >= 2 then 
        local t1, t2 = table.unpack(res)
        res = {}
        if t1 > t2 then t1, t2 = t2, t1 end
        if t2 < 0 then return end
        if t1 > 1 then return end
        if t1 <= 0 then table.insert(res,A) else table.insert(res, A+t1*U) end
        if t2 >= 1 then table.insert(res,B) else table.insert(res, A+t2*U) end
        return res 
    end
end

function clipline(d,xmin,xmax,ymin,ymax)
-- clippe la droite d = {a,u} avec la fenêtre [xmin, xmax]x[ymin, ymax]
-- a est un point de la droite et u un vecteur directeur (2 complexes)
-- et renvoie un segment ou nil
    local res = {}
    local A = d[1]
    local U = d[2]
    local C = { Z(xmin, ymin),
                Z(xmax, ymin),
                Z(xmax, ymax),
                Z(xmin, ymax),
                Z(xmin, ymin) }
    local test1 = cpx.det(A-C[1],U)
    for i = 2, 5 do
        local test2 = cpx.det(A-C[i],U)
        if test1*test2 <= 0 then -- C[i] et C[i-1] sont de part et d'autre de d
            test1 = test2
            local V = C[i]-C[i-1]
            local t = cpx.det(C[i-1]-A,U) / cpx.det(U,V)
            local z = C[i-1] + t*V
            if z ~= nil and (t > 0) then table.insert(res,z) end
        end
    end
    if #res == 2 then return res end
end

function clippolyline(L,xmin,xmax,ymin,ymax,close)
-- L est une liste de complexes ou une liste de liste de complexes
    local clippee = false
    
    local isIn = function(z,i)
        return ((i == 1) and (z.re >= xmin)) or
               ((i == 2)and (z.im >= ymin)) or
               ((i == 3) and (z.re <= xmax)) or
               ((i == 4) and (z.im <= ymax))
    end
    
    local intersect = function(a,b,i) -- intersecte  le segment [a,b] avec le coté i, a et b sont de part et d'autre
        local x
        if i == 1 -- intersection x=xmin
        then x = b.im + (a.im-b.im)*(xmin-b.re)/(a.re-b.re); return Z(xmin,x)
        else
            if i == 2 -- intersection avec y=ymin
            then x = b.re + (a.re-b.re)*(ymin-b.im)/(a.im-b.im); return Z(x,ymin)
            else
                if i == 3 -- intersection avec x=xmax
                then x = b.im + (a.im-b.im)*(xmax-b.re)/(a.re-b.re); return Z(xmax,x)
                else -- intersection avec y=ymax
                    x = b.re + (a.re-b.re)*(ymax-b.im)/(a.im-b.im); return Z(x,ymax)
                end
            end
        end
    end
    
    local clipcp = function(cp) -- clipping d'une composante connexe
        local lastin, lastout, last, r
        local sortie
        for i = 1, 4 do -- on clippe avec les 4 côtés
            last, lastin, lastout, sortie = nil, nil, nil, {}
            for _,z in ipairs(cp) do
                z = toComplex(z)
                if isIn(z,i) then
                    if lastout ~= nil then
                        r = intersect(lastout,z,i)
                        clippee = true
                        if r ~= nil then
                            if last ~= r then table.insert(sortie,r); last = r end
                        end
                        lastout = nil
                    end
                    lastin = z
                    if last ~= z then table.insert(sortie,z); last = z end
                else -- not isIn(z,i)
                    if lastin ~= nil then
                        r = intersect(lastin,z,i)
                        clippee = true
                        if r ~= nil then 
                            if last ~= r then table.insert(sortie,r); last = r end
                        end
                        lastin = nil
                    end
                    lastout = z
                end
            end
            cp = sortie
        end
        return sortie
    end
    -- corps de le fonction clippolyline
    if (type(L) ~= "table") or (#L == 0) then return end
    if (type(L[1]) == "number") or isComplex(L[1]) then L = {L} end
    local res, aux = {}
    for _,cp in ipairs(L) do
        if #cp > 1 then 
            if close then cp = table.copy(cp); table.insert(cp,cp[1]) end
            aux = clipcp(cp) -- on clippe chaque composante connexe
            if (aux ~= nil) and (#aux > 1) then table.insert(res,aux) end
        end
    end
    return res, clippee
end

function clipdots(L,xmin,xmax,ymin,ymax)
-- L est une liste de complexes ou une liste de listes de complexes
-- L est clippée avec la fenêtre [xmin, xmax]x[ymin, ymax]

    local clipcp = function(cp)
        local sortie = {}
        for _, z in ipairs(cp) do
            z = toComplex(z)
            if (z.re >= xmin) and (z.re <= xmax) and (z.im >= ymin) and (z.im <= ymax)
            then table.insert(sortie, z)
            end
        end
        return sortie
    end
    
    if (type(L) ~= "table") or (#L == 0) then return end
    if (type(L[1]) == "number") or isComplex(L[1]) then L = {L} end
    local res, aux = {}
    for _,cp in ipairs(L) do
        aux = clipcp(cp) -- on clippe chaque composante connexe
        if (aux ~= nil) and (#aux > 0) then table.insert(res,aux) end
    end
    return res
end

-- recoller des composantes connexes, utilisé par les courbes implicites
function merge(L)
-- L est une liste de liste de complexes
-- on essaie de recoller au mieux les composantes connexes de L si possible
-- la fonction renvoie le résultat
    local res = {}
    local test = function(t1,t2)
        -- on teste si t1 se recolle à t2, si oui on modifie t1 et on renvoie true, sinon on ne range rien et on renvoie false
        local a1, b1 = t1[1], t1[#t1]
        local a2, b2 = t2[1], t2[#t2]
        if b1 == a2 then --fin de t1 égal début de t2
            for k = 2, #t2 do
                table.insert(t1,t2[k])
            end
            return true
        end
        if b1 == b2 then -- fin de t1 égal fin de t2
            for k = #t2-1, 1, -1 do
                table.insert(t1,t2[k])
            end
            return true
        end
        if a1 == b2 then -- début de t1 égal fin de t2
            for k = #t2-1,1,-1 do
                table.insert(t1,1,t2[k])
            end
            return true
        end
        if a1 == a2 then -- début de t1 égal au début de t2
            for k = 2, #t2 do
                table.insert(t1,1,t2[k])
            end
            return true
        end
        return false
    end
    
    while #L ~= 0 do
        local t1 = table.remove(L) -- on prend le dernier élément de L (et on l'enlève) on va le tester aux autres
        local k = 1
        while k <= #L do --on teste t1 aux autres
            local t2 = L[k]
            if test(t1,t2) then table.remove(L,k); k = 1
            else k = k+1
            end
        end
        table.insert(res,t1) -- on a fait pour les test pour t1,on le range dans res
    end
    return(res)
end

-- constructions de droites

function line(A,B) -- appelée par la méthode Dline
    A, B = toComplex(A), toComplex(B)
    if (A == nil) or (B == nil) then return end
    return {A, B-A}
end

function lineEq(a,b,c) -- appelée par la méthode DlineEq
-- renvoie la droite d'équation ax+by+c=0 sous la forme {A,u}
-- où A est un point de u un vecteur directeur
    if (a == 0) and (b == 0) then return end
    if a ~= 0 then
        return {Z(-c/a,0), Z(-b,a)}
    else
        return { Z(0,-c/b), Z(-b,a) }
    end
end

function perp(d,A) -- appelée par la méthode Dperp
-- perpendiculaire à d passant par a
-- d doit être une droite {point, vecteur directeur} (2 complexes) ou juste un vecteur directeur (1 complexe non nul)
-- la fonction renvoie une droite
    if (d == nil) or (type(d) ~= "table") then return end
    local V
    if #d == 2 then V = d[2] else V = d[1] end
    A = toComplex(A); V = toComplex(V)
    if (A == nil) or (V == nil) or ((V.re == 0) and (V.im == 0)) then return end
    return {A, cpx.I*V}
end 

function parallel(d,A) -- appelée par la méthode Dparallel
-- parallèle à d passant par A
-- d doit être une droite {point, vecteur directeur} (2 complexes) ou juste un vecteur directeur (1 complexe non nul)
-- la fonction renvoie une droite
    if (d == nil) or (type(d) ~= "table") then return end
    local V
    if #d == 2 then V = d[2] else V = d[1] end
    A = toComplex(A); V = toComplex(V)
    if (A == nil) or (V == nil) or ((V.re == 0) and (V.im == 0)) then return end
    return {A, V}
end 

function med(A,B) -- ou med(seg) appelée par la méthode Dmed
-- renvoie la médiatrice du segment [A; B] (2 complexes)
    if B == nil then 
        local seg = A
        if (seg == nil) or (type(seg) ~= "table") or (#seg ~= 2) then return end
        A, B = table.unpack(seg)
    end
    A = toComplex(A); B = toComplex(B)
    if (A == nil) or (B == nil) then return end
    local C = (A+B)/2
    if C == nil then return end
    return {C, cpx.I*(B-A)}
end

function bissec(B,A,C,interior)
-- bissectrice de l'angle géométrique BAC
    A = toComplex(A); B = toComplex(B); C = toComplex(C)
    if interior == nil then interior = true end
    if (A == nil) or (B == nil) or (C == nil) then return end
    local u, v = B-A, C-A
    u = u/cpx.abs(u)
    v = v/cpx.abs(v)
    if (u == nil) or (v == nil) then return end
    if interior then return {A,u+v}
    else 
        if u == v then return {A,cpx.I*u}
        else return {A,v-u}
        end
    end
end

-- contours avec courbes

function domain1(f,a,b,nbdots,discont,nbdiv)
-- renvoie le contour de la partie du plan comprise entre la courbe de f, l'axe Ox, et les droites x=a, x=b 
    local C = cartesian(f,a,b,nbdots,discont,nbdiv)
    if C ~= nil then
        local res = {a}
        for _, cp in ipairs(C) do
            for _,z in ipairs(cp) do
                table.insert(res,z)
            end
        end
        table.insert(res,b)
        return res
    end
end

function domain2(f,g,a,b,nbdots,discont,nbdiv)
-- renvoie le contour de la partie du plan comprise entre la courbe de f, la courbe de g, et les droites x=a, x=b 
    local C = cartesian(f,a,b,nbdots,discont,nbdiv)
    local D = cartesian(g,b,a,nbdots,discont,nbdiv) -- g dans l'autre sens
    if (C ~= nil) and (D ~=nil) then
        local res = {}
        for _, cp in ipairs(C) do
            for _,z in ipairs(cp) do
                table.insert(res,z)
            end
        end
        for _, cp in ipairs(D) do
            for _,z in ipairs(cp) do
                table.insert(res,z)
            end
        end
        return res
    end
end

function domain3(f,g,a,b,nbdots,discont,nbdiv)
-- renvoie le contour de la partie du plan comprise entre la courbe de f, la courbe de g, dans l'intervalle [a,b]
    local C = cartesian(f,a,b,nbdots,discont,nbdiv)
    local D = cartesian(g,b,a,nbdots,discont,nbdiv) -- g dans l'autre sens
    if (C ~= nil) and (D ~= nil) then
        local P = interL(C,D)
        if (P == nil) or (#P <2)  then return end
        local A, B = P[1], P[#P] -- premier et dernier point
        local C1, C2 = {}, {}
        for _, cp in ipairs(C) do
            for _,z in ipairs(cp) do
                table.insert(C1,z)
            end
        end
        for _, cp in ipairs(D) do
            for _,z in ipairs(cp) do
                table.insert(C2,z)
            end
        end
        local res = {A}
        for _, z in ipairs(C1) do
            if (z.re >= A.re) and (z.re <= B.re) then table.insert(res,z) end
        end
        table.insert(res,B)
        for _, z in ipairs(C2) do
            if (z.re >= A.re) and (z.re <= B.re) then table.insert(res,z) end
        end
        return res
    end
end

function roundline(L,r,close,bezier)  -- utilisée par path
-- L est une liste de complexe et r un rayon (réel > 0)
-- la fonction arrondit les angles avec un arc de cercle de rayon r 
-- on renvoie la nouvelle liste de points si bezier vaut false, sinon on renvoie un chemin en courbes de Bézier
    local a, b, c, res = nil, L[1], L[2], {}
    local u, v, error, ok = nil, c-b, false, true
    bezier = bezier or false
    v = v/cpx.abs(v)
    if close then table.insert(L,b); table.insert(L,c); ok = false else table.insert(res,b) end
    for k = 3, #L do
        a = b; b = c; c = L[k] -- on travaille sur l'angle abc
        u = -v
        v = c-b; v = v/cpx.abs(v)
        if(u ~= nil) and (v ~= nil) then
            local w = u+v
            w = w/cpx.abs(w) -- vecteur unitaire de la bissectrice intérieure de  l'angle abc
            local alpha = cpx.arg(w/v) -- angle vw
            local h = math.abs(r/math.sin(alpha))
            local d = math.abs(r/math.tan(alpha))
            if notDef(h) or notDef(d) then error = true
            else
                local center = b + h*w
                local a1, b1 = b+d*u, b+d*v -- projeté du centre sur (ab) et (bc)
                local sens
                if  cpx.det(u,v) > 0 then sens = -1 else sens = 1 end
                if bezier then
                   local C = arcb(a1,center,b1,r,sens)
                    if C ~= nil then
                        table.insert(res,C[1])
                        if ok then table.insert(res,"l") else ok = true end -- pas de "l" après le tout premier si close est true
                        for k = 2, #C do
                            table.insert(res,C[k])
                        end
                    else error = true
                    end
                else
                    local C = arc(a1,center,b1,r,sens)
                    if C ~= nil then
                        for _,z in ipairs(C[1]) do
                            table.insert(res,z)
                        end
                    else error = true
                    end
                end
            end
        else error = true
        end
        if error then table.insert(res,b); error = false end
    end
    if close then 
        if bezier then table.insert(res,"cl") 
        else table.insert(res,res[1]) 
        end
    else 
        table.insert(res,L[#L])
        if bezier then table.insert(res,"l") end
    end
    if #res > 0 then return res end
end

function path(chemin)
-- renvoie les points constituant le chemin
-- celui-ci est une table de complexes et d'instructions ex: {-1,2+i,3,"l", 4, "m", -2*i,-3-3*i,"l","cl",...}
-- "m" pour moveto, "l" pour lineto, "b" pour bézier, "c" pour cercle, "ca" pour arc de cercle, "ea" arc d'ellipse, "e" pour ellipse, "cl" pour close
-- "la" pour line arc (ligne aux coins arrondis), "cla" ligne fermée aux coins arrondis
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
        local C = bezier(table.unpack(aux)) -- renvoie une liste de liste de complexes
        for _, z in ipairs(C[1]) do
            table.insert(crt,z)
        end
        first = last
        aux = {}
    end
    
    local Spline = function()
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        local C = spline(aux) -- renvoie un chemin
        local a, b, c1, c2, i, L
        a = C[1]; i = 2
        if first == nil then table.insert(crt,a) end
        for k = 2, #C do
            if i == 2 then c1 = C[k]; i = 3
            else
                if i == 3 then c2 = C[k]; i = 4
                else
                    if i == 4 then 
                        b = C[k]
                        L = bezier(a,c1,c2,b)
                        for j = 2, #L[1] do
                            table.insert(crt,L[1][j])
                        end
                        a = b
                        i = 5
                    else
                        i = 2 -- passer la lettre "b"
                    end
                end
            end
        end
        first = last
        aux = {}
    end
    
    local Circle = function()
    -- il faut un point et le centre
        if first ~= nil then 
            table.insert(aux,1,first); table.remove(crt)
        end
        local c, v = nil, nil
        if #aux == 2 then -- on a un point et le centre
            c = aux[2]; v = aux[1]-c
        else
            if #aux == 3 then -- on a trois points du cercle
                local a = aux[1]
                c = interD(med(a,aux[2]), med(a,aux[3]))
                if c == nil then aux = {}; return end
                v = a - c
            else aux = {}; return
            end
        end
        local p = function (t)
            return c + v*cpx.exp(cpx.I*t)
        end
        local C = parametric(p,0,2*math.pi)
        if C ~= nil then
            for _, z in ipairs(C[1]) do
                table.insert(crt,z)
            end
            first = crt[#crt]
        end
        aux = {}
    end
    
    local Arc = function()
        local n = #aux
        if (n < 3) or (n > 5) then aux = {}; return end
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        local C = arc(table.unpack(aux))
        if C ~= nil then
            for _, z in ipairs(C[1]) do
                table.insert(crt,z)
            end
            first = crt[#crt]
        end
        aux = {}
    end
    
    local Earc = function() -- ellipticarc(b,a,c,rx,ry,sens,inclin)
        local n = #aux
        if (n < 4) or (n > 7) then aux = {}; return end
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        local C = ellipticarc(table.unpack(aux))
        if C ~= nil then
            for _, z in ipairs(C[1]) do
                table.insert(crt,z)
            end
            first = crt[#crt]
        end
        aux = {}
    end    
    
    local Ellipse = function() -- ellipse(p,c,rx,ry,sens,inclin)
        local n = #aux
        if (n < 3) or (n > 5) then aux = {}; return end
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        local p, c, rx, ry, inclin = table.unpack(aux)
        local C = ellipticarc(p,c,p,rx,ry,1,inclin)
        if C ~= nil then
            for _, z in ipairs(C[1]) do
                table.insert(crt,z)
            end
            first = crt[#crt]
        end
        aux = {}
    end 
    
    local Rline = function(close) --on appelle roundline(L,r)
        local n = #aux
        if (n < 2) then aux = {}; return end
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        local r = table.remove(aux)
        if (type(r) ~= "number") or (r <= 0) then aux = {}; return end
        local C = roundline(aux,r,close)
        if C ~= nil then
            for _, z in ipairs(C) do
                table.insert(crt,z)
            end
            first = crt[#crt]
        end
        aux = {}
    end
    
    local cRline = function()
        Rline(true)
    end
    
    local traiter = { ["s"]=Spline, ["l"]=lineto, ["m"]=moveto, ["cl"]=close, ["b"]=Bezier, ["c"]=Circle, ["ca"]=Arc, ["ea"]=Earc, ["e"]=Ellipse, ["la"]=Rline, ["cla"]=cRline } 
    for _, z in ipairs(chemin) do
        if (type(z) == "number") or isComplex(z) then table.insert(aux,z); last = z 
        else
            if type(z) == "string" then traiter[z]() end
        end
    end
    
    if #crt > 0 then table.insert(res, crt) end    
    if #res > 0 then return  res end
end


-- ensembles
function set(center,angle,scale)
-- renvoie un chemin représentant un ensemble centré sur center, penché de angle degrés
-- scale permet de jouer sur la taille
    center = toComplex(center)
    if center == nil then return end
    angle = angle or 0
    scale = scale or 1
    local a, b, c, d, v = Z(0,4), Z(-3,-1/2), Z(0,-4), Z(3,-1/2), -5
    local S = spline( {a,b,c,d,a},v,v ) -- ensemble de base, chemin constitué de courbes de Bézier
    local M = matrixof( function(z) return center+cpx.exp(cpx.I*angle*math.pi/180)*z*scale end)
    return mtransform(S,M)
end

function inside(I,L)
-- L est une liste de complexes représentant les sommets d'un polygone
-- I est un point, la fonction renvoie true si I est dans le polygone
-- le résultat n'est pas toujours correct si I est sur le bord.
    local close = function(P) -- il faut fermer L si ce n'est pas fait, mais sans modifier L
        local res, aux = {}, {}
        for _, cp in ipairs(P) do
            if cp[1] ~= cp[#cp] then aux = table.copy(cp), table.insert(aux,cp[1]) else aux = cp end
            table.insert(res,aux)
        end
        return res
    end
    I = toComplex(I) 
    if I == nil then return false end
    if (L == nil) or (type(L) ~= "table") then return false end
    local xmin, xmax, ymin, ymax = getbounds(L) -- rectangle englobant
    if (I.re < xmin) or (I.re > xmax) or (I.im < ymin) or (I.im > ymax) then return false end
    if (type(L[1]) == "number") or isComplex(L[1]) -- on a une liste de complexes
    then L = {L} 
    end
    local stop, epsilon = false, 1e-17
    for _,cp in ipairs(L) do
        for _, z in ipairs(cp) do
            z = toComplex(z)
            stop = math.abs(I.im-z.im) > epsilon
        end
    end
    if not stop then return true end -- ligne plate contenant I
    local R = Z(xmin-1,I.im) -- R est sur l'horizontale passant par I et hors de L
    local res = interL({R,I}, close(L))
    if res == nil then return false
    else
        return (#res%2 == 1)
    end
end
