-- luadraw_lines.lua (chargé par luadraw__calc)
-- date 2026/01/15
-- version 2.5
-- Copyright 2026 Patrick Fradin
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
    if #apres == 1 then apres = apres[1] end
    if #avant == 1 then avant = avant[1] end
    if before then return apres, avant 
    else return avant, apres
    end
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
    local angle = cpx.arg((c-a)/(b-a))
    if math.abs(angle) < 1e-12 then angle = 0 end
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

function circumcircle(a,b,c) -- ou circumcircle({a,b,c})
-- renvoie le centre et le rayon du cercle circonscrit
    if b == nil then 
        a, b, c = table.unpack(a)
    end
    local Ce = interD(med(a,b),med(b,c)) -- centre du cercle circonscrit
    if Ce ~= nil then
        return Ce, cpx.abs(Ce-a) -- centre et rayon
    end
end

function incircle(a,b,c) -- ou incircle({a,b,c})
-- renvoie le centre et le rayon du cercle inscrit
    if b == nil then 
        a, b, c = table.unpack(a)
    end
    local Ce = interD(bissec(b,a,c),bissec(a,b,c)) -- centre du cercle inscrit
    if Ce ~= nil then
        local I =  proj(Ce,{a,b-a})
        if I ~= nil then
            return Ce, cpx.abs(Ce - I) -- centre et rayon
        end
    end
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

-- triangles
function sss_triangle(ab,bc,ac)  -- returns 3 points A,B,C (table), the arguments are the lengths of the 3 sides.
    local A, B = 0, ab
    local alpha = math.acos((ab^2+ac^2-bc^2)/(ab*ac*2))
    local C = ac*cpx.exp(cpx.I*alpha)
    return {A,B,C} -- returns triangle with A=0 and B=ab
end

function sas_triangle(ab,gamma,ac)  -- returns 3 points A,B,C (table), the arguments are length, angle (AB,AC) (degrees), length
    local A, B = 0, ab
    local C = ac*cpx.exp(cpx.I*gamma*deg) 
    return {A,B,C} -- returns triangle with A=0 and B=ab
end

function asa_triangle(alpha,ab,beta)  -- returns 3 points A,B,C (table), the arguments are a length, angles (AB,AC) and (BA,BC)
    local A, B = 0, ab
    local D1 = {0,cpx.exp(cpx.I*alpha*deg)}
    local D2 = {ab,cpx.exp(-cpx.I*beta*deg)}
    local C = interD(D1,D2)
    return {A,B,C} -- returns triangle with A=0 and B=ab
end


function polyreg(centre,sommet,nbcotes,sens) -- ou polyreg(sommet1,sommet2,nbcotes) appelée par la méthode Dpolyreg
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

function tangentI(f,x0,y0,long)
-- tangente à la courbe implicite d'équation f(x,y)=0
-- si long vaut nil on renvoie une droite, sinon un segment
-- on suppose que f(x0,y0)=0, donc on a bien un point de la courbe
    if (f == nil) or (x0 == nil) or (y0 == nil) then return end
    local h = 1e-6
    local A = Z(x0,y0)
    local a,b = (f(x0+h,y0)-f(x0-h,y0))/(2*h), (f(x0,y0+h)-f(x0,y0-h))/(2*h)
    local v = Z(-b,a)
    if (v == nil) or cpx.isNul(v) then return end
    if long == nil then return {A,v} -- on renvoie une droite
    else 
        local u = long*v/cpx.abs(v)/2
        return {A-u,A+u} -- on renvoie un segment
    end
end

function tangent_from(from,p,t1,t2,dp)
-- from : point on the plane from which the tangents will originate
-- p: t -> p(t) parameterization of the curve (p(t) is a complex number)
-- t1, t2 : interval boundaries
-- dp (optionnal) derivative function of the function p
-- returns a list of points (points on the curve whose tangent passes through from point)
    local h = 1e-6
    if dp == nil then
        dp = function(t) -- approximate derivative function
            return (p(t+h)-p(t))/h
        end
    end
    local f = function(t) -- function that must be zero
        return cpx.det(p(t)-from,dp(t))
    end
    local n = math.max(25, math.floor((t2-t1)*2.5))
    local S = solve(f,t1,t2-2*h,n) --<< here we use the solve function to solve f(t)=0
    if S ~= nil then 
        local rep = {}
        for _,t in ipairs(S) do
            if cpx.abs(dp(t))>1e-8 then table.insert(rep,p(t)) end -- to avoid singular points
        end
        return rep 
    else return {} -- empty list if no solution is found
    end
end

-- normales

function normal(p,t0,long)
-- normale à la courbe paramétrée par t -> p(t) (à valeurs complexes)
-- au point de paramètre t0
-- si long vaut nil on renvoie une droite, sinon un segment
    if (p == nil) or (t0 == nil) then return end
    local A = p(t0)
    if (A == nil) then return end
    local v = (p(t0+1E-6)-p(t0))*1e6 -- vecteur tangent
    if (v == nil) or cpx.isNul(v) then return end
    v = cpx.I*v
    if long == nil then return {A,v} -- on renvoie une droite
    else 
        local u = long*v/cpx.abs(v)/2
        return {A-u,A+u} -- on renvoie un segment
    end
end

function normalC(f,x0,long)
-- normale à la courbe cartésienne d'équation y=f(x)
-- au point d'abscisse x0
-- si long vaut nil on renvoie une droite, sinon un segment
    if (f == nil) or (x0 == nil) then return end
    local y0 = f(x0)
    local A = Z(x0,y0)
    if (A == nil) then return end
    local v = Z(1, (f(x0+1E-6)-y0)*1E6) -- vecteur tangent
    if (v == nil) or cpx.isNul(v) then return end
    v = cpx.I*v
    if long == nil then return {A,v} -- on renvoie une droite
    else 
        local u = long*v/cpx.abs(v)/2
        return {A-u,A+u} -- on renvoie un segment
    end
end

function normalI(f,x0,y0,long)
-- normale à la courbe implicite d'équation f(x,y)=0
-- si long vaut nil on renvoie une droite, sinon un segment
-- on suppose que f(x0,y0)=0, donc on a bien un point de la courbe
    if (f == nil) or (x0 == nil) or (y0 == nil) then return end
    local h = 1e-6
    local A = Z(x0,y0)
    local a,b = (f(x0+h,y0)-f(x0-h,y0))/(2*h), (f(x0,y0+h)-f(x0,y0-h))/(2*h)
    local v = Z(-b,a)
    if (v == nil) or cpx.isNul(v) then return end
    v = cpx.I*v
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
            local t1 = round(cpx.dot(E-A,u) / cpx.abs2(u),12)
            local t2 = round(cpx.dot(E-C,v) / cpx.abs2(v),12)
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

-- intersection droite - cercle
function interDC(d,C)
-- d = {A,u} droite; C = {O,r} cercle
    local rep 
    local A,u = table.unpack(d)
    local O,r = table.unpack(C)
    local I = proj(O,d)
    local d = cpx.abs(I-O)
    if d == r  then rep = {I}
    elseif d < r then -- deux points d'intersection
        u =  u/cpx.abs(u)
        local b = cpx.dot(u,I-O)
        local delta = math.sqrt(b^2-d^2+r^2)
        local t1, t2 = b-delta, b+delta
        rep = {I+t1*u,I+t2*u}
    end
    return rep
end

-- intersection cercle - cercle
function interCC(C1,C2)
-- C1 = {O1,r1} cercle; C2 = {O2,r2} cercle
    local rep 
    local O1,r1 = table.unpack(C1)
    local O2,r2 = table.unpack(C2)
    if O1 == O2 then
        if r1 == r2 then rep = circle(O1,r1)[1] -- points du cercle
        end
    else
        O1, O2 = toComplex(O1), toComplex(O2)
        local x1,x2,y1,y2 = O1.re,O2.re,O1.im,O2.im
        local d = lineEq(2*(x2-x1),2*(y2-y1),r2^2-r1^2+x1^2-x2^2+y1^2-y2^2)
        rep = interDC(d,C1)
    end
    return rep
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
            if close and (cp[1]~=cp[#cp]) then table.insert(cp,cp[1]) end
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
            aux = clipcp(table.copy(cp)) -- on clippe chaque composante connexe
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

function cutpolyline(L,line,close)
-- cut the polyline L (list of complex numbers or list of lists of complex numbers)
-- with the line, where line = {A,n} (point and a direction vector)
-- the part in the half-plane containing i*n is kept
-- close indicates whether L should be closed
-- the function returns the conserved part (polyline), the non-conserved part (polyline) and the intersection points

    if isComplex(L[1]) then L = {L} end -- pour avoir une liste de listes
    local S,n = table.unpack(line)
    n = cpx.I*n -- vecteur normal à la droite
    close = close or false
    local res, res2, coupe = {}, {}, {} -- res = sortie, res2 = autre partie 
    for _,F in ipairs(L) do
        if close then table.insert(F,F[1]) end
        local nb, aux, aux2 = #F, {}, {}
        local A1, B1 = nil, F[1]
        local p1, p2, I = nil, cpx.dot(B1-S,n)
        if math.abs(p2)<1e-8 then p2 = 0 end
        if (p2 >= 0) then table.insert(aux,B1) end
        if (p2 <= 0) then table.insert(aux2,B1) end
        for k = 2, nb do
            --if k == nb+1 then k = 1 end -- on ferme le polygon
            A1 = B1; p1 = p2; B1 = F[k]; p2 = cpx.dot(B1-S,n)
            if math.abs(p2) < 1e-8 then p2 = 0 end
            if (p1*p2 < 0) or (p2 == 0) then
                if p2 == 0 then I = B1 else I = projO(A1,line,B1-A1) end
                if I ~= nil then 
                    table.insert(aux,I) ; table.insert(aux2,I)
                    table.insert(coupe,I)
                end
            end
            if (p2 > 0) and (p1 ~= nil) then table.insert(aux,B1) end
            if (p2 < 0) and (p1 ~= nil) then table.insert(aux2,B1) end
        end
        if aux[1] == aux[#aux] then table.remove(aux) end
        if aux2[1] == aux2[#aux2] then table.remove(aux2) end
        if #aux>1 then table.insert(res,aux) end
        if #aux2>1 then table.insert(res2,aux2) end
    end
    return res, res2, coupe -- returns the two polygons and intersection points
end


-- recoller des composantes connexes, utilisé par les courbes implicites
function merge(List)
-- L est une liste de liste de complexes
-- on essaie de recoller au mieux les composantes connexes de L si possible
-- la fonction renvoie le résultat
    local res = {}
    local L = table.copy(List)
    local equal = function(z1,z2)
        return z1==z2 --cpx.abs(z1-z2) < 1e-12
    end
    local test = function(t1,t2)
        -- on teste si t1 se recolle à t2, si oui on modifie t1 et on renvoie true, sinon on ne range rien et on renvoie false
        local a1, b1 = t1[1], t1[#t1]
        local a2, b2 = t2[1], t2[#t2]
        if equal(b1,a2) then --fin de t1 égal début de t2
            for k = 2, #t2 do
                table.insert(t1,t2[k])
            end
            return true
        end
        if equal(b1,b2) then -- fin de t1 égal fin de t2
            for k = #t2-1, 1, -1 do
                table.insert(t1,t2[k])
            end
            return true
        end
        if equal(a1,b2) then -- début de t1 égal fin de t2
            for k = #t2-1,1,-1 do
                table.insert(t1,1,t2[k])
            end
            return true
        end
        if equal(a1,a2) then -- début de t1 égal au début de t2
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
        table.insert(res,t1) -- on a fait les test pour t1,on le range dans res
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

function path(chemin,nbdots)
-- renvoie les points constituant le chemin
-- celui-ci est une table de complexes et d'instructions ex: {-1,2+i,3,"l", 4, "m", -2*i,-3-3*i,"l","cl",...}
-- "m" pour moveto, "l" pour lineto, "b" pour bézier, "c" pour cercle, "ca" pour arc de cercle, "ea" arc d'ellipse, "e" pour ellipse, "cl" pour close
-- "la" pour line arc (ligne aux coins arrondis), "cla" ligne fermée aux coins arrondis
-- nbdots (optionnel) est utilisé pour la conversion bézier -> ligne polygonale
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
        local a,c1,c2,b = table.unpack(aux)
        local C = bezier(a,c1,c2,b,nbdots) -- renvoie une liste de listes de complexes
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
                        L = bezier(a,c1,c2,b,nbdots)
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

function polyline2path(L) -- conversion list of complex numbers or a list of lists of  complex numbers (L) to path
    if (L==nil) or (type(L) ~= "table") or (#L == 0) then return end
    if (type(L[1]) == "number") or isComplex(L[1]) then L = {L} end
    local ret = {} 
    local aux
    for _, cp in ipairs(L) do
        aux = table.copy(cp)
        table.insert(aux,2,"m") -- move
        table.insert(aux,"l")  -- lineto
        insert(ret,aux)
    end
    return ret
end


-- ensembles (diagrammes de Venn), intersection, réunion, différence
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

function cap(C1, C2) -- contour de l'intersection de C1 et C2
-- C1 et C2 sont deux courbes fermées simples (représentant des ensembles)
-- la fonction renvoie le contour de l'intersection (liste de complexes)
    local L = interL(C1,C2)
    if L == nil then
        if inside(C1[1],C2) then return C1
            elseif inside(C2[1],C1) then return C2
            else return 
        end
    end
    local rep = {}
    local L1 = L[1] -- premier point commun, on réorganise C1 et C2 pour commencer par L1
    local C1B, C1A = cut(C1,L1) -- C1B = avant L1, C1A = après L1
    table.remove(C1A) --on enlève le dernier
    C1 = concat(C1A, C1B)
    local C2B, C2A = cut(C2,L1) -- C2B = avant L1, C2A = après L1
    table.remove(C2A) --on enlève le dernier
    C2 = concat(C2A, C2B) 
    -- on parcourt L par paire (L1,L2)
    local L2 = table.remove(L,1)
    table.insert(L,L1)
    local aux, aux2, A, k = {}, {}
    for j, z in ipairs(L)  do
        L1 = L2; L2 = z
        k = 1; A = C1[2]
        while (cpx.abs(L1-A) < 0.1) and (k<#C1) do k=k+1; A = C1[k] end -- point suivant L1
        if inside(A,C2) then -- on prend la partie [L1,L2] de C1
            aux = cut(C1,L2)
        else -- on prend la partie [L1,L2] de C2
            aux = cut( cut(C2,L1,true), L2)
            if aux == nil then
                aux2 = cut(C2,L1,true)
                table.remove(aux2)
                aux = concat(aux2, cut(C2,L2))
            end
            
        end
        table.remove(aux); C1 = cut(C1,L2,true) -- on coupe C1 avant L2
        insert(rep,aux)
    end
    table.insert(rep,C2[1])
    return rep
end

function cup(C1, C2)
-- C1 et C2 sont deux courbes fermées simples (listes de complexes, représentant des ensembles)
-- la fonction renvoie le contour de la réunion (liste de complexes)
    local L = interL(C1,C2)
    if L == nil then -- disjoints
        if isComplex(C1[1]) then C1 = {C1} end
        if isComplex(C2[1]) then C2 = {C2} end
        return concat(C1,C2)
    end
    local rep = {}
    local L1 = L[1] -- premier point commun, on réorganise C1 et C2 pour commencer par L1
    local C1B, C1A = cut(C1,L1) -- C1B = avant L1, C1A = après L1
    table.remove(C1A) --on enlève le dernier
    C1 = concat(C1A, C1B)
    local C2B, C2A = cut(C2,L1) -- C2B = avant L1, C2A = après L1
    table.remove(C2A) --on enlève le dernier
    C2 = concat(C2A, C2B) 
    -- on parcourt L par paire (L1,L2)
    local L2 = table.remove(L,1)
    table.insert(L,L1)
    local aux, aux2, A, k = {}, {}
    for _, z in ipairs(L)  do
        L1 = L2; L2 = z
        k = 1; A = C1[2]
        while (cpx.abs(L1-A) < 0.1) and (k<#C1) do k=k+1; A = C1[k] end -- point suivant L1
        --local A = C1[2] -- point suivant L1, est-il dans C2 ?
        if inside(A,C2) then -- on prend la partie [L1,L2] de C2
            aux = cut( cut(C2,L1,true), L2)
            if aux == nil then
                aux2 = cut(C2,L1,true)
                table.remove(aux2)
                aux = concat(aux2, cut(C2,L2))
            end
        else -- on prend la partie [L1,L2] de C1
            aux = cut(C1,L2)
        end
        table.remove(aux); C1 = cut(C1,L2,true) -- on coupe C1 avant L2
        insert(rep,aux)
    end
    table.insert(rep,C2[1])
    return rep
end


function setminus(C1, C2)
-- C1 et C2 sont deux courbes fermées simples (listes de complexes, représentant des ensembles)
-- la fonction renvoie le contour de C1-C2
    local L = interL(C1,C2)
    if L == nil then -- disjoints
        if isComplex(C1[1]) then C1 = {C1} end
        if isComplex(C2[1]) then C2 = {C2} end
        return concat(C1,C2)
    end
    local rep = {}
    local L1 = L[1] -- premier point commun, on réorganise C1 et C2 pour commencer par L1
    local C1B, C1A = cut(C1,L1) -- C1B = avant L1, C1A = après L1
    table.remove(C1A) --on enlève le dernier
    C1 = concat(C1A, C1B)
    local C2B, C2A = cut(C2,L1) -- C2B = avant L1, C2A = après L1
    table.remove(C2A) --on enlève le dernier
    C2 = concat(C2A, C2B) 
    -- on parcourt L par paire (L1,L2)
    local L2 = table.remove(L,1)
    table.insert(L,L1)
    local aux, aux2, A, k = {}, {}
    for _, z in ipairs(L)  do
        L1 = L2; L2 = z
        k = 1; A = C1[2]
        while (cpx.abs(L1-A) < 0.1) and (k<#C1) do k=k+1; A = C1[k] end -- point suivant L1
        if inside(A,C2) then -- on prend la partie [L2,L1] de C2
            aux = cut( cut(C2,L2,true), L1)
            if aux == nil then
                aux2 = cut(C2,L1)
                table.remove(aux2)
                aux = concat(aux2, cut(C2,L2,true))
            end
            aux = reverse(aux)
        else -- on prend la partie [L1,L2] de C1
            aux = cut(C1,L2)
        end
        table.remove(aux); C1 = cut(C1,L2,true) -- on coupe C1 avant L2
        insert(rep,aux)
    end
    table.insert(rep,C2[1])
    return rep
end

-- enveloppe convexe 2d

function cvx_hull2d(L)
-- L is a list of complex numbers
-- returns a list of complexe numbers which is the convex hull of L (Ronald Graham algorithm)
    if (L == nil) or (type(L) ~= "table") then return end
    L = map(toComplex,L)
    -- on élimine les doublons
    table.sort(L, function(e1,e2) return (e1.re < e2.re) or ((e1.re == e2.re) and (e1.im < e2.im)) end)
    local old, S = L[1],{L[1]}
    for k = 2, #L do -- on élimine les doublons
        local z = L[k]
        if not cpx.isNul(z-old) then table.insert(S,z); old = z end
    end
    local Min = S[1]
    local N  = #S 
    if N < 3 then  return S
    elseif N == 3 then 
        if cpx.det(S[2]-Min,S[3]-Min) < 0 then return reverse(S) else return S end
    else
        -- recherche de l'élément le plus bas, le plus à gauche s'il y en a plusieurs
        local ymin = Min.im
        for _,z in ipairs(S) do
            local y = z.im 
            if y < ymin then Min = z; ymin = y
            elseif y == ymin then
                  if z.re < Min.re then Min = z end
            end
        end  -- Min est le point le plus bas, le plus à gauche s'il y en a plusieurs 
        -- pour chaque z autre que Min on calcule l'angle theta = (Ox, z-Min) pour faire un tri
        local aux = {}
        for k = 1, #S do
            local z = S[k]
            local theta = cpx.arg(z-Min)
            if not cpx.isNul(z-Min) then table.insert(aux, {theta,k}) end -- angle et index dans la liste S, sauf Min
        end 
        table.sort(aux,function(e1,e2) return (e1[1] < e2[1]) or ((e1[1]==e2[1]) and (e1[2]<e2[2])) end)
        local S1 = {Min}
        for _,elt in ipairs(aux) do 
            table.insert(S1,S[elt[2]])
        end
        table.insert(S1,Min)
        -- S1 est la liste triée, elle commence et finit par Min
        -- on parcourt les points de S1 sans qu'il y ait de "virage à droite"
        local fin = false 
        local A, B, C, kb = S1[1], S1[2], S1[3], 2 -- kb = index de B
        while not fin  do
            if cpx.det(B-A,C-A) < 1e-12 then -- virage à droite 
                table.remove(S1,kb); kb = kb -1  -- on exclut B de l'enveloppe
                B = A; 
                if kb > 1 then A = S1[kb-1] -- on recule d'un cran
                else fin = true
                end
            else A = B; B = C; kb = kb +1 
                if kb+1 > #S1 then fin = true
                else C = S1[kb+1] --on avance
                end
            end
        end
        table.remove(S1) -- on retire Min qui est en dernier
        return S1 -- S1 contient l'enveloppe
    end
end

-- conversion ligne polygonale -> bande
function line2strip(L,wd,closed,ends)
-- L is a list of complex numbers or a list of list od complex numbers
-- wd is the width of the strip (cm)
-- closed boolean indicating whether the line should be closed
-- ends boolean indicating whether the two end segments should be drawn
-- retrns a path
    if (L == nil) or (type(L) ~= "table") then return end
    local ep = wd/2
    local i = cpx.I
    closed = closed or false
    if closed then ends = false end
    if ends == nil then ends = true end
    if (type(L[1]) == "number") or isComplex(L[1]) then L = {L} end
    local ret, aux = {}
    local bord, dessus, first, a, b, c, u, v, w
    for _, cp1 in ipairs(L) do
        aux = {}
        local cp = table.copy(cp1)
        a, b = cp[1], cp[2]
        while a == b do table.remove(cp,1); b = cp[2] end
        local cycle = (a==cp[#cp])
        local close = closed or cycle
        table.remove(cp,1); table.remove(cp,1)
        if close then
            if not cycle then table.insert(cp,a) end
            a = (a+b)/2
            table.insert(cp,a)
        end
        v = i*(b-a)/cpx.abs(b-a)
            bord = {a-ep*v,a+ep*v}; dessus= {bord[2],"l"}
        first = bord[1]; table.insert(aux,first); table.insert(aux,"m")
        c = b; b = a; v = v/i
        for _, z in ipairs(cp) do
            a = b; b = c; c = z; u  =-v; v = cpx.normalize(c-b)
            if v == nil then
                c = b; b = a; v = -u
            else
                w = cpx.normalize(u+v)
                if w == nil then
                    bord = {b+ep*i*u, b-ep*i*u}
                else
                    bord = projO( bord,{b,w},u)
                end
                table.insert(aux,bord[1]); table.insert(dessus,1,bord[2])
            end
        end
        if ends then
            aux = concat(aux,{c-ep*v*i, c+ep*v*i}, dessus,"cl")
            insert(ret, aux)
        else
            insert(aux,{c-ep*i*v,"l"}); insert(dessus,{c+ep*i*v,"m"},1)
            ret = concat(ret, aux,dessus)
        end
    end
    return ret
end

-- construire une ligne polygonale parallèle
function parallel_polyline(L,wd,closed)
-- L is a list of complex numbers or a list of list od complex numbers
-- wd is the distance between the two lines
-- closed boolean indicating whether the line should be closed
-- returns a polyline
    if (L == nil) or (type(L) ~= "table") then return end
    local ep = wd
    local i = cpx.I
    closed = closed or false
    if (type(L[1]) == "number") or isComplex(L[1]) then L = {L} end
    local ret, aux = {}
    local bord, dessus, first, a, b, c, u, v, w
    for _, cp1 in ipairs(L) do
        aux = {}
        local cp = table.copy(cp1)
        a, b = cp[1], cp[2]
        while a == b do table.remove(cp,1); b = cp[2] end
        local cycle = (a==cp[#cp])
        local close = closed or cycle
        table.remove(cp,1); table.remove(cp,1)
        if close then
            if not cycle then table.insert(cp,a) end
            a = (a+b)/2
            table.insert(cp,a)
        end
        v = i*(b-a)/cpx.abs(b-a)
            bord = a+ep*v; dessus= {bord}
        c = b; b = a; v = v/i
        for _, z in ipairs(cp) do
            a = b; b = c; c = z; u  =-v; v = cpx.normalize(c-b)
            if v == nil then
                c = b; b = a; v = -u
            else
                w = cpx.normalize(u+v)
                if w == nil then
                    bord = b-ep*i*u
                else
                    bord = projO( bord,{b,w},u)
                end
                table.insert(dessus,bord)
            end
        end
        table.insert(dessus,c+ep*i*v)
        table.insert(ret,dessus)
    end
    return ret
end


-- triangulation de Delaunay, algorithme de Bowyer-Watson
function delaunay(points, out)  -- points is a list of distinct complex numbers
-- renvoie une liste de triangles { {u,v,w}, ... }
-- out doit être une variable égale à une table vide,  elle recevra les données pour Voronoi (liste de triangles avec centre du cercle circoncrit)
    local superTri = function(points)
        --superTri(points) : renvoie un triangle contenant la liste de points
        local x1,x2,y1,y2 = getbounds(points)
        local a, c = Z(x1,y1) - 100*(x2-x1)*Z(1,1), Z(x2,y2) + 100*(y2-y1)*Z(1,1) -- on agrandit la boite
        local d, b = Z(a.re,c.im), Z(c.re,a.im)
        local A = (c+b)/2+c-d
        local B, C = interD({A,c-A},{a,d-a}), interD({A,b-A},{a,d-a}) --triangle englobant la grande boite
        return {A,B,C}
    end
    
    local add_contour = function(T,contour) -- T = triangle, contour = liste d'arêtes = {{a,b},{c,d},...}
        --insère les aretes du triangle dans la liste, si elle est déjà présente, il faut la supprimer
        local b, a, A, index = T[1], nil
        for k = 1, 3 do  -- pour chaque arête du triangle
            a = b; b = T[k%3+1]
            local ok, j, N = true, 0, #(contour)
            while ok and (j < N) do -- cette arête est-elle déjà dans le contour ?
                j = j+1
                A = contour[j]
                if ((a == A[1]) and (b == A[2])) or ((a == A[2]) and (b == A[1])) then 
                    ok = false
                    index = j
                end
            end
            if not ok then --l'arete est déjà dans le contour, on la supprime
               table.remove(contour,index)
            else --sinon on l'ajoute au contour
                table.insert(contour,{a,b})
            end
        end
        return contour
    end

    local function contains(list, value)
        for _, v in ipairs(list) do
            if v == value then
                return true
            end
        end
        return false
    end

    points = map(toComplex,points)
    table.sort(points, function(e1,e2) return (e1.re < e2.re) or ((e1.re == e2.re) and (e1.im < e2.im)) end) --tri suivant les x croissants
    local T = superTri(points) --triangle englobant le nuage
    local triangles = { {T,circumcircle(T)} } -- premier triangle avec centre et rayon du cercle circonscrit
    for _,sommet in ipairs(points) do  -- on traite sommet par sommet
        local contour = {}  -- contour défini par les "mauvais" triangles (polygone connexe)
        local index = 0        -- indice du triangle traité
        local a_supprimer = {} -- liste des indices des triangles à supprimer
        for _,z in ipairs(triangles) do --on balaie chaque triangle
            index = index +1  -- numéro du triangle traité
            if cpx.abs(sommet-z[2])<z[3] then  --sommet dans le cercle circonscrit => mauvais triangle
                contour = add_contour(z[1],contour) --calcul du nouveau contour (=liste d'arêtes)
                table.insert(a_supprimer,index) --il faudra supprimer de triangle
            end
        end
        local newtriangles= {}
        for k,z in ipairs(triangles) do --suppression des mauvais triangles
            if not contains(a_supprimer,k) then table.insert(newtriangles,z) end
        end
        triangles = newtriangles
        --Chaque arête du contour donne un nouveau triangle en ajoutant le sommet en cours
        for _,z in ipairs(contour) do
            table.insert(z,sommet)
            table.insert(triangles,{z,circumcircle(z)})
        end
    end
    --finalisation : supprimer les triangles contenant un des sommets du premier triangle T (superTri)
    local rep = {}
    for _,z in ipairs(triangles) do
        local t = z[1]
        if not  (contains(t,T[1]) or contains(t,T[2]) or contains(t,T[3])) then
           table.insert(rep,t) --on insert un triangle de Delaunay
           if out ~= nil then table.insert(out,{z[1],z[2]}) end -- triangle et centre du cercle circonscrit
        end
    end
    return rep
end

function voronoi(points,window)
-- renvoie une liste de {centre, cellule de Voronoi (polygone)} correspondant aux points de la liste.
    local L, polyList = {}, {}
    local aux = delaunay(points,L)
    window = window or {-5,5,-5,5}
    local x1,x2,y1,y2 = table.unpack(window)
    local contour = {Z(x1,y1),Z(x2,y1),Z(x2,y2),Z(x1,y2)}
    -- il faut que la boite [x1;x2]x[y1;y2] contienne les triangles, et les centres des cercles circonscrits
    x1,x2,y1,y2 = getbounds(concat(points,contour,map(function(Z) return Z[2] end, L))) 
    local ep = 1 -- il ne faut pas de centre de cercle circonscrit sur le contour
    contour = {Z(x1-ep,y1-ep),Z(x2+ep,y1-ep),Z(x2+ep,y2+ep),Z(x1-ep,y2+ep),Z(x1-ep,y1-ep)}
    local diam = cpx.abs(Z(x2-x1,y2-y1))
    
    local Sort = function(edge)
        table.sort(edge,function(z1,z2) return (z1.re<z2.re) or ((z1.re==z2.re) and (z1.im<z2.im)) end)
    end 
      
    local equal = function(e1,e2) -- égalié d'arêtes triées
        --return (cpx.abs(e1[1]-e2[1])<1e-8) and (cpx.abs(e1[2]-e2[2])<1e-8)
        return (e1[1]==e2[1]) and (e1[2]==e2[2])
    end  
      
    local inTri = function(edge,triangle)
    -- renvoie true si on a une arête du triangle
        local a, b, c = table.unpack(triangle)
        local e1,e2,e3 = {a,b},{a,c},{b,c}
        Sort(e1); Sort(e2); Sort(e3)
        return equal(edge,e1) or equal(edge,e2) or equal(edge,e3)
    end
    
    local close_cell = function(cell)
        --fermer une cellule non bornée de Voronoi en fonction de la fenetre window.
        local deb, fin = cell[1], cell[#cell]
        local d = 2*diam
        local u = deb-cell[2]; u = d*cpx.normalize(u)
        local v = fin-cell[#cell-1]; v = d*cpx.normalize(v)
        local x = sym(cell,{deb+u,fin+v-deb-u})
        local aux = concat(deb+u,cell,fin+v,reverse(x))
        return clippolyline(aux,x1,x2,y1,y2,true)[1]
    end
    
    local classify = function(sommet,edge,out)
    --pour obtenir à la fin une liste de cellules de Voronoi autour de chaque sommet. out doit être une variable table, elle est mise à jour.
        local edgeIn = function(edge, edge_list)
        --si edge est dans edge_list
            for _,elt in ipairs(edge_list) do
                if equal(edge,elt) then return true end
            end
            return false
        end
        
        local present = false
        for _,cp in ipairs(out) do -- cp={sommet, liste d'arêtes}
            if (not present) and (cp[1]==sommet) then -- sommet déjà dans la liste
                present = true
                if not edgeIn(edge,cp[2]) then -- arête non présente dans la liste
                    table.insert(cp[2],edge)
                end
            end
        end
        if not present then --sommet non present dans la liste
            table.insert(out,{sommet,{edge}})
        end
    end
    
    --corps de la fonction voronoi
    for num,z in ipairs(L) do
        local T = z[1] -- triangle de Delaunay
        insert(T,table.copy(T))
        local C = z[2] -- centre du cercle ciconscrit
        for j = 1, 3 do -- parcours par arête
            local A = {T[j],T[j+1]}; Sort(A)
            local op = T[j+2] -- sommet opposé à A
            local ok, tri, seg = false
            for k, t in ipairs(L) do
                if (k ~= num) and inTri(A,t[1]) then -- il ne faut pas prendre le triangle en cours
                    ok = true; tri = t; break
                end
            end
            if ok then -- arete commune avec le triangle tri
                -- le segment joignant les deux centres est une arête commune aux cellule de A[1] et de A[2]
                seg = {C, tri[2]}; Sort(seg)
                classify(A[1],seg,polyList) --arête de la cellule correspondant au point A[1]
                classify(A[2],seg,polyList) --arête de la cellule correspondant au point A[2]
            else -- arête A non commune avec un autre triangle, on a une cellule non bornée
                local V = A[1]
                local n = cpx.I*(A[2]-V) -- vecteur normal à l'arête A
                local mil = (V+A[2])/2
                local U, B = mil-C
                if cpx.abs(U) < 1e-8 then 
                    U = n; if cpx.dot(op-V,n)>0 then U = -U end
                end
                U = diam*cpx.normalize(U)                
                if cpx.dot(op-V,n)*cpx.dot(C-V,n) >= 0 then -- C et op du même côté de l'arête A
                    B = interL({C,C+U}, contour)[1]
                else
                    B = interL({C,C-U}, contour)[1]
                end
                seg = {C,B}; Sort(seg)
                classify(A[1],seg,polyList)
                classify(A[2],seg,polyList)
            end
        end
    end
    local ret, poly = {}
    for k,Z in ipairs(polyList) do
        poly = merge(Z[2])[1] -- on fusionne la liste d'arêtes en un polygone
        if poly[1] ~= poly[#poly] then -- polygone non fermé, la cellule est non bornée
            poly = close_cell(poly)
        else table.remove(poly)
        end
        table.insert(ret, {Z[1], poly}) -- on insère le point et sa cellule de Voronoi
    end
    return ret
end
