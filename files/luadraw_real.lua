-- luadraw_real.lua (chargé par luadraw_complex.lua)
-- date 2025/07/04
-- version 2.0
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

digits = 4 -- nombre de décimales dans les exports
epsilon = 1e-16
mm = math.floor(7227/254) -- conversion en dixième de point, ex: Linewidth(2*mm) pour une épaisseur de 2 millimètres
deg = math.pi/180 -- conversion en radians, ex 180*deg = pi
rad = 180/math.pi -- conversion en degrés, ex : pi*rad = 180

function strReal(reel) -- convertit un réel en chaîne avec digits décimales
    local str = string.format("%." ..digits.."f",reel)
    local n = #str
    while string.sub(str,n,n) == "0" do
        n = n-1
    end
    if string.sub(str,n,n) == "." then n = n-1 end
    return string.sub(str,1,n)
end

function isNaN(reel)  -- reel est supposé être un nombre
    return reel ~= reel
end

function isInf(reel) -- reel est supposée être un nombre
    return (reel == math.huge) or (reel == -math.huge)
end

function notDef(reel) -- reel est supposée être un nombre
    return isNaN(reel) or isInf(reel)
end 

function isNul(reel)
    return math.abs(reel) < epsilon
end

function round(num, nbDeci)
  local mult = 10^(nbDeci or 0)
  return math.floor(num * mult + 0.5) / mult
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

function reverse(tbl)
-- renvoie la séquence inverse de tbl (tbl n'est pas modifiée)
    local rep = {}
    for k = #tbl, 1, -1 do
        table.insert(rep,tbl[k])
    end
    return rep
end

function gcd(a,b)
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

function lcm(a,b)
    return math.abs(a*b) // gcd(a,b)
end

function simplifyFrac(a,b)
-- renvoie la fraction d'entiers a/b simplifiée
    local sg = 1
    if ((a < 0) and (b > 0)) or ((a > 0) and (b < 0)) then sg = -1 end
    a, b = math.abs(a), math.abs(b)
    if (math.floor(a) == a) and (math.floor(b) == b) then 
        local d = gcd(a,b)
        return sg*a//d, b//d
    else return sg*a, b
    end
end

function addFrac(a,b,c,d)
-- renvoie la somme a/b + c/d sous forme de fraction
-- a, b, c, d sont supposés être des entiers avec b et d non nuls.
    local num, den = a*d+b*c, b*d
    return simplifyFrac(num, den)
end

function gradLabel(a,b,text,dollar)
-- mise en forme d'un label pour une graduation : a*text/b, renvoie une chaîne
-- dollar = true/false indique s'il faut des dollars ou non
    function label(x)
            local str
            if type(x) == "number" then str = strReal(x) else str = x end
            if dollar then return "$"..str.."$"
            else return str
            end
        end
    if a == 0 then return label(0) -- \fraction nulle
    else
        if text == "" then -- pas de text
            if b == 1 then return label(a)
            else
                if a > 0 then
                    if dollar then return "$\\frac{".. strReal(a).."}{"..strReal(b).."}$" 
                    else return strReal(a).."/"..strReal(b) 
                    end 
                else -- a < 0
                    if dollar then return "$-\\frac{".. strReal(-a).."}{"..strReal(b).."}$" 
                    else return strReal(a).."/"..strReal(b) 
                    end
                end
            end
        else -- text non vide
            if b == 1 then
                        if a == -1 then return label("-"..text)
                        else 
                            if  a == 1 then return label(text)
                            else return label(strReal(a)..text)
                            end
                        end
            else -- b différent de 1 
                if  a > 0 then 
                        if dollar then 
                            if a ~= 1 then return "$\\frac{"..strReal(a)..text.."}{"..strReal(b).."}$"
                            else return "$\\frac{"..text.."}{"..strReal(b).."}$"
                            end
                        else 
                            if a ~= 1 then return strReal(a)..text.."/"..strReal(b)
                            else return text.."/"..strReal(b)
                            end
                        end 
                else  -- a < 0    
                        if dollar then 
                            if a ~= -1 then return "$-\\frac{"..strReal(-a)..text.."}{"..strReal(b).."}$"
                            else return "$-\\frac{"..text.."}{"..strReal(b).."}$"
                            end
                        else 
                            if a ~= -1 then return strReal(a)..text.."/"..strReal(b)
                            else return "-"..text.."/"..strReal(b)
                            end
                        end 
                end
            end
        end
    end
end


function solve(f,a,b,n)
-- résout numériquement f(x)=0 dans l'intervalle [a,b], celui-ci est divisé en n morceaux.
-- f doit être une fonction, a et b deux réels.
    if (f == nil) or (a == nil) or (type(a) ~= "number") or (b == nil) or (type(b) ~= "number") then return end
    n = n or 25
    if a > b then a, b = b, a end
    local x, delta, S = a, (b-a)/n, {}
    local fin, h, r = x+delta, 1e-6, nil
    while x < b do
        r = x+delta/2
        for i = 1,5 do
            if r ~= nil then r = r - f(r)*h/(f(r+h)-f(r)) end
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
