-- luadraw_curves.lua (chargé par luadraw__calc)
-- date 2026/01/15
-- version 2.5
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

bezier_nbdots = 12 -- Minimum number of points calculated when converting a Bézier curve into a polygonal line.
function parametric(p,t1,t2,nbdots,discont,nbdiv) --new version, experimantal
-- le paramétrage p est une fonction : t (réel) -> p(t) (cpx)
    local saut = (discont or false)
    local niveau = (nbdiv or 5)
    local curve = {}
    local lastJump = true
    local dep = t1
    local fin = t2
    local nb = (nbdots or 40)
    local pas = (fin - dep) / (nb-1)
    local cp = {} -- composante connexe courante
    local t = dep
    local count = 0
    local seuil = math.abs(pas)*5 --empirique

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
    local middle
    middle = function(t1,t2,f1,f2,n,d12) -- nouvelle fonction récursive middle, expérimentale
        if (n > niveau) then 
            if saut then closeCp() end -- fermer la composante connexe
        else
            local tm = (t1 + t2) / 2  -- dichotomie
            local fm = p(tm)
            if (fm ~= nil) then
                if (f1 ~= nil) and (f2 ~= nil) then
                    if d12 == nil then d12 = cpx.abs(f2-f1) end
                    local d1m, dm2 = cpx.abs(f1-fm), cpx.abs(f2-fm)
                    if (d12>seuil) or (d1m+dm2>1.0005*d12) -- d(f1,f2)>seuil ou d(f1,fm)+d(fm,2)>1.0005*d(f1,f2)
                    then
                        middle(t1,tm,f1,fm,n+1,d1m)
                        addCp(fm)
                        middle(tm,t2,fm,f2,n+1,dm2)
                    end
                else
                    middle(t1,tm,f1,fm,n+1,nil)
                    addCp(fm)
                    middle(tm,t2,fm,f2,n+1,nil)
                end
            else -- fm = nil
                if (f1 ~= nil) then middle(t1,tm,f1,fm,n+1,nil) else closeCp() end
                if (f2 ~= nil) then middle(tm,t2,fm,f2,n+1,nil) else closeCp() end
            end
        end        
    end
    --corps de la fonction parametric
    local prec = nil -- point précédent le point courant
    local tPrec = nil -- valeur de t précédente
    local z = nil -- point courant
    for _ = 1, nb do
        z = evalf(p,t)
        if (z == nil) or notDef(z.re) or notDef(z.im) then z = nil end
        if (z ~= nil) then
            if (prec ~= nil) or ( (prec == nil) and (t > dep) ) then -- and test(tPrec,t,prec,z)
                middle(tPrec,t,prec,z,1,nil)
            end
            addCp(z)
        else -- z = nil
            if (prec ~= nil) then middle(tPrec,t,prec,z,1,nil) 
            else closeCp()
            end
        end
        tPrec = t
        prec = z
        t = t + pas
    end
    if (not lastJump) and (count > 1) then table.insert(curve, cp) end -- dernière composante
    if #curve > 0 then return curve end
end

function OLDparametric(p,t1,t2,nbdots,discont,nbdiv) 
-- le paramétrage p est une fonction : t (réel) -> p(t) (cpx)
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
           local fm = evalf(p,tm)
           if (fm ~= nil) then
                if (f1 == nil) or (cpx.N1(f1-fm) > seuil) then
                    middle(t1,tm,f1,fm,n+1)
                end
                addCp(fm)
                if (f2 == nil) or (cpx.N1(f2-fm) > seuil) then
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
        z = evalf(p,t)
        if (z == nil) or notDef(z.re) or notDef(z.im) then z = nil end
        if (z ~= nil) then
            if ( (prec ~= nil) and (cpx.N1(prec-z) > seuil) ) or ( (prec == nil) and (t > dep) ) then
                middle(tPrec,t,prec,z,1)
            end
            addCp(z)
        else -- z = nil
            if (prec ~= nil) then middle(tPrec,t,prec,z,1) 
            else closeCp()
            end
        end
        tPrec = t
        prec = z
        t = t + pas
    end
    if (not lastJump) and (count > 1) then table.insert(curve, cp) end -- dernière composante
    if #curve > 0 then return curve end
end

function polar(rho,t1,t2,nbdots,discont,nbdiv) -- le paramétrage rho(t) renvoie un réel
    local p = function (t)
        return Zp(rho(t),t) 
    end
    return parametric(p,t1,t2,nbdots,discont,nbdiv)
end   

function cartesian(f,x1,x2,nbdots,discont,nbdiv)
-- f doit être une fonction x (réel) -> f(x) (réel)
    local p = function (x)
        return Z( x, f(x) ) 
    end
    return parametric(p,x1,x2,nbdots,discont,nbdiv)
end

 
function periodic(f,period,x1,x2,nbdots,discont,nbdiv)
-- courbe d'une fonction f périodique de période period={a,b}
-- f doit être une fonction x (réel) -> f(x) (réel)
    local p = function (x)
        return Z( x, f(x) )
    end
    local tmin, tmax = period[1], period[2]
    local T = tmax - tmin
    local C = parametric(p,tmin,tmax,nbdots,discont,nbdiv)
    if C == nil then return end
    local k1, k2 = math.floor( (x1-tmin)/T ), math.floor( (x2-tmax)/T )+1
    local rep = {}
    for k = k1, k2 do
        for _,cp in ipairs(C) do
            local aux = {}
            for _,z in ipairs(cp) do
                table.insert(aux, z+k*T)
            end
            table.insert(rep, aux)
        end
    end
    rep = cutpolyline(rep,{x1,-cpx.I}); rep = cutpolyline(rep,{x2,cpx.I})
    return rep
end


function affinebypiece(def,discont)
-- courbe d'une fonction affine par morceaux
-- def est une table permettant la définition de la fonction def = { {x1,x2,x3,...,xn}, {{a1,b1}, {a2,b2},...} }
--{x1,x2,...,xn} forme une subdivision de l'intervalle [x1,xn] où est calculée la courbe
-- sur l'intervalle [x1,x2] l'équation est y=a1*x+b1, sur [x2,x3] c'est y=a2*x+b2, etc
    if discont == nil then discont = true end -- true par défaut
    local sub, val = def[1], def[2]
    local n = #sub -1
    local res, crt, last = {}, {}, nil
    for k = 1, n do
        local x1, x2 = sub[k], sub[k+1]
        local a, b = table.unpack(val[k])
        local y1, y2 = a*x1+b, a*x2+b
        if last == y1 then -- continuité en x1 
            table.insert(crt, Z(x2,y2))
        else 
            if #crt > 0 then  
                if discont then table.insert(res,crt)
                else Insert(res,crt)
                end
                crt = {} 
            end
            table.insert(crt, Z(x1,y1))
            table.insert(crt, Z(x2,y2))
        end
        last = y2
    end
    if #crt > 0 then  
        if discont then table.insert(res,crt)
        else Insert(res,crt)
        end
    end    
    if #res > 0 then return res end
end

function stepfunction(def,discont)
-- courbe d'une fonction en escalier
-- def est une table permettant la définition de la fonction def = { {x1,x2,x3,...,xn}, {c1,c2,...} }
--{x1,x2,...,xn} forme une subdivision de l'intervalle [x1,xn] où est calculée la courbe
-- sur l'intervalle [x1,x2] la fonction vaut c1, sur [x2,x3] c'est c2, etc
    if discont == nil then discont = true end -- true par défaut
    local sub, val, last = def[1], def[2], nil
    local n = #sub -1
    local res, crt = {}, {}
    for k = 1, n do
        local a, b, c = sub[k], sub[k+1], val[k] 
        if last == c then -- continuité en x1 
            table.insert(crt, Z(b,c))
        else 
            if #crt > 0 then  
                if discont then table.insert(res,crt)
                else Insert(res,crt)
                end
                crt = {} 
            end
            table.insert(crt, Z(a,c))
            table.insert(crt, Z(b,c))
        end
        last = c
    end
    if #crt > 0 then  
        if discont then table.insert(res,crt)
        else Insert(res,crt)
        end
    end    
    if #res > 0 then return res end
end

function implicit(f,x1,x2,y1,y2,grid)
-- renvoie une liste de segments constituant la courbe implicite d'équation f(x,y)=0 dans le pavé [x1,x2]X[y1,y2]
-- grid = {n1, n2} ce qui signifie que l'intervalle [x1,x2] est découpé en n1 morceaux, et l'intervalle [y1,y2] en n2 morceaux.
    local tolerance = 1e-6
    
    local dist = function(a,b)
            return cpx.abs(b-a)
        end
    
    local dicho = function(a,b) -- a,b sont deux tables { Z(x,y),f(x,y)}  retourne "none" ou un point
            local z1, z2, f1, f2 = a[1], b[1], a[2], b[2]
            if f1*f2 <= 0 then
                for _ = 1, 10 do --while cpx.abs(z2-z1) >= tolerance do
                    local z3 = (z1+z2)/2
                    local fm = f(z3.re,z3.im)
                    if f1*fm <= 0 then  z2 = z3 else z1 = z3; f1=fm end
                end
                return (z1+z2)/2
            end
            return "none"
        end
    
    local  N, P = grid[2], grid[1]    
    local xpas, ypas = (x2-x1)/P, (y2-y1)/N
    local Grid = {} -- grille de points { Z(xi,yj), f(xi,yj) }
    local y = y1    
    for _ = 1, N+1 do
        local ligne = {}
        local x = x1
        for _ = 1, P+1 do
            table.insert(ligne, {Z(x,y), f(x,y)} )
            x = x+xpas
        end
        table.insert(Grid, ligne)
        y = y+ypas
    end
    local AretesV = {}
    for I = 1, N do
        local cp = {}
        for J = 1, P+1 do
            table.insert(cp, dicho( Grid[I][J], Grid[I+1][J]) )  -- arête verticale
        end
        table.insert(AretesV, cp)
    end
    local AretesH = {}
    for I = 1, N+1 do
        local cp = {}
        for J = 1, P do
            table.insert(cp, dicho( Grid[I][J], Grid[I][J+1]) )  -- arête horizontale
        end
        table.insert(AretesH, cp)        
    end
    local rep = {}
    for I = 1,N do
        for J = 1, P do
            local m1 = AretesV[I][J] -- point sur arête verticale gauche de la maille
            local m2 = AretesH[I][J] --point sur arête horizontale basse de la maille
            local m3 = AretesV[I][J+1] --point sur arête verticale droite de la maille
            local m4 = AretesH[I+1][J] --point sur arête horizontale haute de la maille
            local m5 = true --(Grid[I][J+1][2]*Grid[I+1][J][2] <=0 ) -- test changement de signe sur diag descendante
            local m6 = true --(Grid[I][J][2]*Grid[I+1][J+1][2] <= 0)
            if m1 ~= "none" then
                if (m2 ~= "none") and m6 then
                    table.insert( rep, {m1,m2} )
                else if (m3 ~= "none") and ((m5) or (m6)) then
                        table.insert( rep, {m1,m3} )
                    else if (m4 ~= "none") and m5 then
                            table.insert( rep, {m1,m4} )
                         end
                    end
                end
            end
            if m2 ~= "none" then
                if (m3 ~= "none") and m5 then
                    table.insert( rep, {m3,m2} )
                else    if (m4 ~= "none") and ((m5) or (m6)) then
                            table.insert( rep, {m4,m2} )
                        end
                end
            end
            if (m3 ~= "none") then
                if (m4 ~= "none") and m6 then
                   table.insert( rep, {m3,m4} )
                end
            end
        end
    end
    if #rep > 0 then return merge(rep) end -- on renvoie le résultat en recollant les segments avec merge
end


function odeRK4(f,t0,Y0,tmin,tmax,nbdots)
-- résolution dans l'intervalle [tmin,tmax] (contenant t0) de Y'(t)=f(t,Y(t)) où f: (t,Y) -> f(t,Y) dans R^n avec Y(t)={y1(t),...,yn(t)} (liste de réels)
--t0 et Y0 donnent les conditions initiales, nbdots le nombre de points calculés (50 par défaut)
-- la fonction renvoie une liste { {tmin,...,tmax}, {y1(tmin),..., y1(tmax)}, ..., {yn(tmin),..., yn(tmax)} }
-- méthode Runge Kutta 4
    local calcError = false
    local ode_f
    if type(Y0) == "number" then -- dimension 1
        ode_f = function(t,Y) 
            if Y == nil then calclError = true; return 
            else return {f(t,Y[1])} end
        end
        Y0 = {Y0}
    else
        ode_f = function(t,Y) 
            if Y == nil then calclError = true; return 
            else return f(t,Y) end
        end
    end
    local add = function(Y1,Y2) -- addition dans R^n
            if calcError then return end
            if (Y1 == nil) or (Y2 == nil) then calcError = true; return end
            local rep = {}
            for k,y in ipairs(Y1) do
                local r = y+Y2[k]
                if notDef(r) then calcError = true; return end
                table.insert(rep,r)
            end
            return rep
        end
    local mul = function(k,Y) -- produit par un scalaire
            local rep = {}
            if calcError then return end
            if (Y == nil) then calcError = true; return end
            for _,y in ipairs(Y) do
                local r = k*y
                if notDef(r) then calcError = true; return end
                table.insert(rep,r)
            end
            return rep
        end
    local h = (tmax-tmin)/(nbdots-1) 
    local t, Y, n, stop = t0, Y0, #Y0, (t0 >= tmax)
    local rep = { {t0} }
    for k = 1, n do
        table.insert(rep,{Y0[k]})
    end
    while not stop do
        local k1 = ode_f(t, Y)
        t = t + h/2 
        local k2 = ode_f(t, add(Y, mul(h/2,k1)) )
        local k3 = ode_f(t, add(Y, mul(h/2,k2)) ) 
        t = t + h/2
        local k4 = ode_f(t, add(Y, mul(h,k3)) ) 
        Y = add(Y, mul(h/6, add(k1, add( mul(2,k2), add( mul(2,k3),k4) ) ) ) )
        if not calcError then 
            table.insert( rep[1], t)
            for k = 1, n do
                table.insert(rep[k+1],Y[k])
            end
        end
        if (t < tmax) and (t+h > tmax) then h = tmax-t end
        stop = (t+h > tmax) or calcError
    end
    calcError = false
    t = t0; Y = Y0;  h = (tmin-tmax)/(nbdots-1); stop = (t0 <= tmin) 
    while not stop do
        local k1 = ode_f(t, Y)
        t = t + h/2 
        local k2 = ode_f(t, add(Y, mul(h/2,k1)) )
        local k3 = ode_f(t, add(Y, mul(h/2,k2)) ) 
        t = t + h/2
        local k4 = ode_f(t, add(Y, mul(h,k3)) ) 
        Y = add(Y, mul(h/6, add(k1, add( mul(2,k2), add( mul(2,k3),k4) ) ) ) ) 
        if not calcError then 
            table.insert(rep[1],1,t)
            for k = 1, n do
                table.insert(rep[k+1],1,Y[k])
            end
        end
        if (t>tmin) and (t+h < tmin) then h = tmin-t end
        stop = (t+h < tmin) or calcError
    end
    return rep
end

function odeRkf45(f,t0,Y0,tmin,tmax,nbdots)
-- résolution dans l'intervalle [tmin,tmax] (contenant t0) de Y'(t)=f(t,Y(t)) où f: (t,Y) -> f(t,Y) dans R^n avec Y(t)={y1(t),...,yn(t)} (liste de réels)
--t0 et Y0 donnent les conditions initiales, nbdots le nombre de points calculés (50 par défaut)
-- la fonction renvoie une liste { {tmin,...,tmax}, {y1(tmin),..., y1(tmax)}, ..., {yn(tmin),..., yn(tmax)} }
-- méthode de Runge-Kutta-Fehlberg (pas variable)
    local calcError = false
    local ode_f
    if type(Y0) == "number" then -- dimension 1
        ode_f = function(t,Y) 
            if Y == nil then calclError = true; return 
            else return {f(t,Y[1])} end
        end
        Y0 = {Y0}
    else
        ode_f = function(t,Y) 
            if Y == nil then calclError = true; return 
            else return f(t,Y) end
        end
    end
    
    local add = function(Y1,Y2) -- addition dans R^n
            if calcError then return end
            if (Y1 == nil) or (Y2 == nil) then calcError = true; return end
            local rep = {}
            for k,y in ipairs(Y1) do
                local r = y+Y2[k]
                if notDef(r) then calcError = true; return end
                table.insert(rep,r)
            end
            return rep
        end
    local mul = function(k,Y) -- produit par un scalaire
            local rep = {}
            if calcError then return end
            if (Y == nil) then calcError = true; return end
            for _,y in ipairs(Y) do
                local r = k*y
                if notDef(r) then calcError = true; return end
                table.insert(rep,r)
            end
            return rep
        end
        
    local Nmax, hMax = 5*nbdots, (tmax-tmin)/(nbdots-1) 
    local hMin = (tmax-tmin)/(Nmax-1) 
    local t, Y, n, stop, err = t0, Y0, #Y0, (t0 >= tmax)
    local Nmax1, Nmax2 = math.floor((tmax-t0)/hMin), math.floor((t0-tmin)/hMin)
    local h, eps, N = hMax, 5e-7, 0
    local rep = { {t0} }
    for k = 1, n do
        table.insert(rep,{Y0[k]})
    end
    local nextY = function()
        local k1 = mul(h, ode_f(t, Y))
        local k2 = mul(h, ode_f(t+0.25*h, add(Y, mul(1/4,k1))))
        local k3 = mul(h, ode_f(t+0.375*h, add(Y, mul(3/32, add(k1,mul(3,k2))))))
        local k4 = mul(h, ode_f(t+12*h/13, add(Y, mul(1/2197, add( mul(1932,k1), add(mul(-7200,k2),mul(7296,k3)))))))         
        local k5 = mul(h, ode_f(t+h, add(Y, add(mul(439/216,k1), add(mul(-8,k2), add(mul(3680/513,k3),mul(-845/4104,k4)))))))
        local k6 = mul(h, ode_f(t+h/2, add(Y, add(mul(-8/27,k1), add(mul(2,k2), add(mul(-3544/2565,k3), add(mul(1859/4104,k4),mul(-11/40,k5))))))))
        Y = add(Y, add(mul(25/216,k1), add(mul(1408/2565,k3), add(mul(2197/4104,k4),mul(-1/5,k5)))))
        err = add(mul(1/360,k1), add(mul(-128/4275,k3), add(mul(-2197/75240,k4), add(mul(1/50,k5),mul(2/55,k6)))))
    end
    -- côté droit de t0
    while not stop do
        nextY()
        t = t+h; N = N+1
        if not calcError then 
            table.insert( rep[1], t)
            for k = 1, n do
                table.insert(rep[k+1],Y[k])
            end
            local d = 0
            for _,x in ipairs(err) do
                if math.abs(x) > d then d = math.abs(x) end
            end
            d = 2*d
            local s = 0
            if d>0 then s = (eps*h/d)^0.25 end
            if s < 0.1 then s = 0.1
            elseif s > 4 then s = 4
            end
            h = s*h
            if h < hMin then h = hMin 
            elseif h > hMax then h = hMax
            end
        end
        if (t < tmax) and (t+h > tmax) then h = tmax-t end
        stop = (t+h > tmax) or (N > Nmax1) or calcError
    end
    -- maintenant, côté gauche de t0
    calcError = false
    t = t0; Y = Y0;  h = -hMax; N = 0; stop = (t0 <= tmin)
       while not stop do
        nextY()
        t = t+h; N = N+1
        if not calcError then 
            table.insert( rep[1],1, t)
            for k = 1, n do
                table.insert(rep[k+1],1,Y[k])
            end
            local d = 0
            for _,x in ipairs(err) do
                if math.abs(x) > d then d = math.abs(x) end
            end
            d = 2*d
            local s = 0
            if d>0 then s = (-eps*h/d)^0.25 end
            if s < 0.1 then s = 0.1
            elseif s > 4 then s = 4
            end
            h = s*h
            if -h < hMin then h = -hMin 
            elseif -h > hMax then h = -hMax
            end
        end
        if (t>tmin) and (t+h < tmin) then h = tmin-t end
        stop = (t+h < tmin) or (N > Nmax2) or calcError
    end
    return rep
end

function odesolve(f,t0,Y0,tmin,tmax,nbdots,method)
    if (method == nil) or (method == "rkf45") then 
        return odeRkf45(f,t0,Y0,tmin,tmax,nbdots)
    elseif method == "rk4" then
        return odeRK4(f,t0,Y0,tmin,tmax,nbdots)
    end
end

function bezier(a,c1,c2,b,nbdots)
--renvoie les points de la courbe de Bézier allant de a à b ayant comme points de contrôle c1 et c2
-- a,c1,c2,b sont des complexes.
    a = toComplex(a)
    c1 = toComplex(c1)
    c2 = toComplex(c2)
    b = toComplex(b)
    if (a == nil) or (b == nil) or (c1 == nil) or (c2 == nil) then return end
    local u, v, w = b-3*c2+3*c1-a, 3*a-6*c1+3*c2, 3*c1-3*a
    local p = function(t)
            return a+t*(w+t*(v+t*u))
        end
    return parametric(p,0,1,nbdots or bezier_nbdots,false,2)
end

function spline(liste,v1,v2)
-- liste : liste de points
-- v1 et v2 vecteurs tangents(complexes) aux extrémités ou bien nil pour une spline naturelle
-- renvoie un chemin à dessiner avec Dpath
    local error = false
    local n = #liste
    if n <= 1 then return end
    local sortie = {}
    n = 2*n-2
    local a, b, c, d = {},{},{},{}
    for k = 1, n-1 do
       table.insert(a,0); table.insert(c,0)
    end
    for k = 1, n do
        table.insert(b,0); table.insert(d,0)
    end
    b[1] = 1
    if v1 == nil then b[n] = 0 else b[n] = 1 end
    for k = 1, (n-2)//2 do 
       b[2*k] = 3; b[2*k+1] = 2 
    end
    if v2 == nil then c[1] = 3 else c [1] = 2 end
    for k = 1, (n-2)//2 do 
        c[2*k] = 1; c[2*k+1] = 5 
    end
    a[n-1] = 1
    for k = 1, (n-2)//2 do 
        a[2*k-1] = 5; a[2*k] = 1 
    end
    local d1, d2, d3 = nil, liste[1], liste[2]
    local k = n-1
    local res1, res2, res3
    if v1 == nil then d[n] = Z(0,0) 
    else
        res1 = d3-d2
        res2 = res1-v1
        if res2 ~= nil then d[n] = res2 else return end
    end
    for j = 3, #liste do
        d1 = d2; d2 = d3; d3 = liste[j]
        res1 = d3-d2
        res2 = d2-d1
        res3 = res1-res2
        if res3 ~= nil then
            d[k] = res3
            res3 = 3*res3
            d[k-1] = res3
        else
          return
        end
        k =k-2
    end
    if v2 == nil then d[1] = Z(0,0)
    else
        res1 = d3-d2 -- d2-d1
        res2 = v2-res1
        if res2 ~= nil then d[1] = res2 else return end
    end
    local u
    for k = 1, n-1 do
        u = a[k]/b[k]
        b[k+1] = b[k+1]-u*c[k]
        d[k+1] = d[k+1]-u*d[k]
    end
    d[n] = d[n] / b[n]
    for k =(n-1), 1, -1 do
        d[k] = (d[k]-c[k]*d[k+1])/b[k]
    end
    for k = n, 1, -1 do table.insert(sortie,d[k]) end
    local A1, A2, a1, a2, C1 = nil, liste[1]
    local res, first = {}, true
    for j = 2, #liste do
        A1 = A2;  A2 = liste[j]; a1 = sortie[2*j-3]; a2 = sortie[2*j-2]
        u = A2-A1-a2-a1
        C1 = u/3+A1
        table.insert(res,A1)
        if not first then table.insert(res,"b") else first = false end
        table.insert(res,C1)
        table.insert(res,a2/3+2*C1-A1)
    end
    table.insert(res,A2); table.insert(res,"b")
    return res
end

function tcurve(L)
-- renvoie sous forme de chemin une courbe passant par des points donnés avec des tangentes imposées à gauche et à droite.
-- L = {pt1, {t1,a1,t2,a2}, pt2, {t1,a1,t2,a2}, ... }
-- t1 est la norme du vecteur tangent à gauche, a1 est l'angle en degré par rapport à l'horizontale du vecteur tangent à gauche
-- c'est la même chose pour le vecteur tangent à droite avec t2 et a2, mais ceux-ci sont facultatifs
-- lorsque t2 et a2 ne sont pas donnés, alors ils prennent les mêmes valeurs que t1 et a1
    local Vdir = function(t1,a1,t2,a2)
        local left 
        if t1 == nil then left = 0
        else
            left = t1*cpx.exp(cpx.I*a1*math.pi/180)-- vecteur tangent gauche
        end
        local right
        if t2 == nil then right = left
        else
            right = t2*cpx.exp(cpx.I*a2*math.pi/180)
        end
        return left,right -- vecteurs tangents gauche et droit
    end
    local a, val, var, b, vbl, vbr = nil, nil, nil, L[1], Vdir(table.unpack(L[2]))
    local res = {b} -- premier point
    for k = 2, #L/2 do
        a, val, var, b, vbl, vbr = b, vbl, vbr, L[2*k-1], Vdir(table.unpack(L[2*k]))
        insert(res,{a+var/3,b-vbl/3,b,"b"}) -- courbe de Bézier de a à b respectant les contraintes
    end
    return res
end

function curvilinear_param(L,close) -- curvilinear parametrization
-- L is a list of complex numbers
-- close=true/false, true if L must be closed
-- returns a function f:t -> f(t) with t in [0;1] and f(t) is a point on L, f(0) is the first point, f(1) the last point.
    if (L == nil) or (type(L) ~= "table") or (#L == 0) then return end
    if not isComplex(L[1])  then L = L[1] end -- liste de liste de complexes, on prend la première composante
    close = close or false
    local a, b, n, p = nil, toComplex(L[1]), #L
    local L2, L1, s = {b}, {0}, 0 -- L2=points, L1 = length    
    if close then p = n+1 else p = n end
    for k = 2, p do
        a = b; b = toComplex(L[(k-1)%n+1])
        s = s + cpx.abs(b-a) -- curve length from beginning to b
        table.insert(L2,b); table.insert(L1,s)
    end
    local total_len = s
    local l = function(t) -- returns L(t) using linear interpolation with t in [0,1]
        local k = 1
        local n = math.min(#L1,#L2)
        if t == 0 then return L2[1] end -- first point of L
        if t == 1 then return L2[#L2] end -- last point
        t = t*total_len
        while (k<=n) and (L1[k] < t) do k = k+1 end
        if k <=n then
            local u = (t-L1[k-1])/(L1[k]-L1[k-1])
            return L2[k-1]+u*(L2[k]-L2[k-1])
        end
    end
    return l,total_len
end
