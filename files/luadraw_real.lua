-- luadraw_real.lua (chargé par luadraw_complex.lua)
-- date 2026/02/17
-- version 2.6
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

luadraw = {}
digits = 4 -- nombre de décimales dans les exports
epsilon = 1e-12
mm = math.floor(7227/254) -- conversion en dixième de point, ex: Linewidth(2*mm) pour une épaisseur de 2 millimètres
pt = 254/7227  -- conversion en cm, ex : 2*pt pour une longueur en cm équivalente à 2 points
deg = math.pi/180 -- conversion en radians, ex 180*deg = pi
rad = 180/math.pi -- conversion en degrés, ex : pi*rad = 180
siunitx = false  -- utilisé dans la fonction num


function strReal(reel,dgt) -- convertit un réel en chaîne avec dgt décimales
    dgt = dgt or digits
    local str = string.format("%." ..dgt.."f",reel)
    local n = #str
    while string.sub(str,n,n) == "0" do
        n = n-1
    end
    if string.sub(str,n,n) == "." then n = n-1 end
    local rep = string.sub(str,1,n)
    if rep == "-0" then rep = "0" end
    return rep
end

function num(x,dgt) -- x is a real, returns a string
    local rep = strReal(x,dgt)
    if siunitx then rep = "\\num{"..rep.."}" end --needs \usepackage{siunitx}
    return rep
end

function isNaN(reel)  -- reel est supposé être un nombre
    return reel ~= reel
end

function isInf(reel) -- reel est supposée être un nombre
    return (reel == math.huge) or (reel == -math.huge)
end

function notDef(reel) -- reel est supposée être un nombre
    return (reel==nil) or isNaN(reel) or isInf(reel)
end 

function isNul(reel)
    return math.abs(reel) < epsilon
end

function round(num, nbDeci)
  local mult = 10^(nbDeci or 0)
  return math.floor(num * mult + 0.5) / mult
end

function nearest(x) -- nearest integer
    local a, b = math.floor(x), math.ceil(x)
    if x <= (a+b)/2 then return a else return b end
end

function range(a, b, step)
    step = step or 1
    local res = {}
    for k = a, b, step do
        table.insert(res,k)
    end
    return res
end

function linspace(a,b,nbdots)
    local res = {}
    nbdots = nbdots or 50
    local pas = (b-a)/(nbdots-1)
    local x = a
    for _ = 1, nbdots do
        table.insert(res,x)
        x = x+pas
    end
    return res
end

function reverse(list)
    local rep = {}
    for _,x in ipairs(list) do
        table.insert(rep,1,x)
    end
    return rep
end

function luadraw.gcd(a,b)
    a = math.abs(a)
    b = math.abs(b)
    local igcd
    igcd = function(u,v)
            if v == 0 then return u
            else return igcd(v, u%v)
            end
        end
    return igcd(a,b)
end

gcd = luadraw.gcd

function lcm(a,b)
    return math.abs(a*b) // luadraw.gcd(a,b)
end

function solve(f,a,b,n,df) -- version 2.4 of luadraw
    n = n or 25
    if a > b then a, b = b, a end
    local x, delta, S = a, (b-a)/n, {}
    local fin, h, r = x+delta, 1e-6, nil
    if df == nil then
        df = function(x)
            return (f(x+h)-f(x))/h
        end
    end
    local iter = function(x)
        return x-f(x)/df(x)
    end
    while x < b do
        r = x+delta/2
        for i = 1,5 do
            if r ~= nil then r = evalf(iter,r) end
            if (r==nil) then break end
        end
        if (r ~= nil) and (r >= x) and (r < fin) and (math.abs(f(r)) < 1e-6) then
            table.insert(S,r) -- on considère que r est une solution
        end
        x = fin; fin = x+delta
    end
    if #S > 0 then return S end
end

function int(f,a,b)
-- calcule numériquement l'intégrale de la fonction f de a à b,
-- f est une fonction à variable réelle mais peut être à valeurs complexes
-- a et b sont deux réels.
    if (f == nil) or (a == nil) or (type(a) ~= "number") or (b == nil) or (type(b) ~= "number") then return end
    local h, deuxN, compt = b-a, 1, 0
    local M0, M1, S0, S1, U0, U1, V0, V1
    local stop, error = false, false
    
    local median = function()
        local x = a+h/2
        local r, y = f(x)
        for k = 1, deuxN-1 do
            x = x+h; y = f(x)
            if y ~= nil then r = r+y else error = true; return end
        end
        return h*r
    end
    
    S1 = h*(f(a)+f(b)+4*f((a+b)/2))/6
    M1 = median(); stop = (M1 == nil) or (S1 == nil)
    while (not stop) and (compt <= 10) do
        compt = compt+1; M0 = M1; S0 = S1; U0 = U1; V0 = V1
        deuxN = 2*deuxN; h = h/2
        M1 = median()
        if not error then S1 = S0/2+2*M1/3-M0/6; error = (S1 == nil) end
        if not error then U1 = (16*S1-S0)/15; error = (U1 == nil) end
        if (not error) and (compt > 1) then V1 = (64*U1-U0)/63; error = (V1 == nil) end
        stop = error or ( (compt >= 3) and (V0 ~= nil) and (V1 ~= nil) and (cpx.abs(V0-V1) < 1e-6) )
    end
    if not error then return V1 end
end
