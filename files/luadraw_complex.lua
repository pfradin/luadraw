--- luadraw_complex.lua
-- date 2026/06/13
-- version 3.2
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl

require 'luadraw_real'
local ld = luadraw

local complex = {}
complex.__index = complex

--- Constructeur
function complex:new(re, im)
    local cplx = {}
    cplx.re = re
    cplx.im = im
    setmetatable(cplx, self)  -- obligatoire, permet d'utiliser self
    if (re == nil) or (im == nil) or ld.notDef(re) or ld.notDef(im) then return 
    else return cplx
    end
end

-- Fonction pour tester si une variable est de type Complex
-- ATTENTION : la fonction renvoie false pour les réels !
function complex.isComplex(a)
    return getmetatable(a) == complex
end

-- Pour transformer un nombre en complexe si possible
function complex.toComplex(a)
    if complex.isComplex(a) then return a 
    else
        if type(a) == "number" then return complex:new(a,0) end
    end
end    

-- Méthode pour ajouter, surcharge de +
function complex.__add(u,v)
    u = complex.toComplex(u)
    v = complex.toComplex(v)
    if (u == nil) or (v == nil) then return end
    return complex:new(u.re + v.re, u.im + v.im)
end


--- Méthode pour soustraire deux nombres complexes, surcharge de -
function complex.__sub(u,v)
    u = complex.toComplex(u)
    v = complex.toComplex(v)
    if (u == nil) or (v == nil) then return end
    return complex:new(u.re - v.re, u.im - v.im)
end

--- Méthode pour multiplier deux nombres complexes, surcharge de *
function complex.__mul(u,v)
    u = complex.toComplex(u)
    v = complex.toComplex(v)
    if (u == nil) or (v == nil) then return end
    return complex:new(u.re * v.re - u.im * v.im, u.re * v.im + u.im * v.re)
end

--- Méthode pour diviser deux nombres complexes, surcharge de /
function complex.__div(u,v)
    u = complex.toComplex(u)
    v = complex.toComplex(v)
    if (u == nil) or (v == nil) then return end
    local denominateur = v.re * v.re + v.im * v.im
    if ld.notDef(denominateur) then return
    else
        return complex:new(
        (u.re * v.re + u.im * v.im) / denominateur,
        (u.im * v.re - u.re * v.im) / denominateur)
    end
end

--- Méthode pour tester l'égalité deux nombres complexes, surcharge de =
function complex.__eq(u,v)
    u = complex.toComplex(u)
    v = complex.toComplex(v)
    if (u == nil) or (v == nil) then return end
    return complex.isNul(u-v) -- égalité à epsilon près
end

function complex.equal(u,v)
    u = complex.toComplex(u)
    v = complex.toComplex(v)
    if (u == nil) or (v == nil) then return end
    return (u.re == v.re) and (u.im == v.im)
end

-- opposé (surcharge de l'opérateur '-c')
function complex.__unm(u)
    u = complex.toComplex(u)
    if (u == nil) then return end    
    return complex:new(-u.re, -u.im)
end

---------------- fonctions mathématiques pour nombres complexes 
--  module
function complex.abs(u)
    if type(u) == "number" then return math.abs(u) end
    u = complex.toComplex(u)
    if (u == nil) then return end
    local x, y = math.abs(u.re), math.abs(u.im)
    if x < y then x, y = y, x end
    if x == 0 then return 0 else return x*math.sqrt(1 + (y/x)^2) end
end

function complex.abs2(u) -- module au carré
    if type(u) == "number" then return math.abs(u) end
    u = complex.toComplex(u)
    if (u == nil) then return end
    return u.re^2 + u.im^2
end

-- argument principal 
function complex.arg(u)
    u = complex.toComplex(u)
    if (u == nil) then return end    
    local x, y = u.re, u.im
    if (x == 0) and (y == 0) then return 0 end -- argument indéfini, 0 par convention
    if (x == 0) then
        if (y > 0) then return math.pi/2 else return -math.pi/2 end
    else
        if (x > 0)  then return math.atan(y / x) 
        else
            if (y >= 0) then return math.pi + math.atan(y / x)
            else return -math.pi + math.atan(y / x)
            end
        end
    end
end

function complex.angle(u,v) -- return angle between vectors u and v (radians)
    u = complex.toComplex(u)
    v = complex.toComplex(v)
    if (u == nil) or (v == nil) then return end  
    return complex.arg(v/u)
end

-- conjugué 
function complex.bar(u)
    u = complex.toComplex(u)
    if (u == nil) then return end    
    return complex:new(u.re, -u.im)
end

-- exponentielle 
function complex.exp(u)
    u = complex.toComplex(u)
    if (u == nil) then return end    
    local r = math.exp(u.re)
    return complex:new( r*math.cos(u.im), r*math.sin(u.im) )
end

function complex.cosh(u)
    u = complex.toComplex(u)
    if (u == nil) then return end    
    local r1, r2 = complex.exp(u), complex.exp(-u)
    return (r1+r2)/2
end

function complex.sinh(u)
    u = complex.toComplex(u)
    if (u == nil) then return end    
    local r1, r2 = complex.exp(u), complex.exp(-u)
    return (r1-r2)/2
end

local powint
powint = function(z,n)
    if n == 0 then return 1
    elseif n < 0 then
        return powint(1/z,-n)
    elseif n%2 == 0 then
        return powint(z*z,n//2)
    elseif n == 1 then return z
    else
        return z*powint(z*z,n//2)
    end
end

function complex.pow(z,n)
    if type(n) ~= "number" then return end
    z = complex.toComplex(z)
    if complex.isNul(z) then
        if n > 0 then return 0 else return end
    end
    if math.floor(n) == n then
        return powint(z,n)
    else
        local r, theta = complex.abs(z), complex.arg(z)
        if theta ~= nil then
            return Zp(r^n,n*theta)
        end
    end
end

-- arrondir
function complex.round(u, nbDeci)
    u = complex.toComplex(u)
    if (u == nil) then return end    
    return complex:new( round(u.re,nbDeci), round(u.im,nbDeci) )
end

-- normaliser
function complex.normalize(z)
    if z == nil then return end
    z = complex.toComplex(z)
    if not complex.isNul(z) then return z/complex.abs(z) end
end

-- norme 1
function complex.N1(u)
    u = complex.toComplex(u)
    if type(u) == "number" then return math.abs(u)
    else return math.abs(u.re) + math.abs(u.im)
    end
end

-- produit scalaire
function complex.dot(u,v)
    u = complex.toComplex(u)
    v = complex.toComplex(v)
    if (u == nil) or (v == nil) then return end    
    local rep = u.re*v.re + u.im*v.im
    if ld.isNul(rep) then return 0 -- nullité à epsilon près
    else return rep
    end
end

-- déterminant    
function complex.det(u,v)
    u = complex.toComplex(u)
    v = complex.toComplex(v)
    if (u == nil) or (v == nil) then return end        
    local rep = u.re*v.im - u.im*v.re
    if ld.isNul(rep) then return 0 -- nullité à epsilon près
    else return rep
    end
end

function complex.isNul(u)
    u = complex.toComplex(u)
    if (u == nil) then return
    else return ld.isNul(u.re) and ld.isNul(u.im)
    end
end    
-------------------------- Méthodes 
function complex:tostring()
    local str = ""
    local x = self.re
    local y = self.im
    
    if ld.notDef(x) or ld.notDef(y) then return "nan" end
    if (x == 0) and (y == 0) then return "0" end
    if x == 1e+308 then return "jump" end
    if x > 0 then str = str..x 
    elseif x < 0 then str = "-"..math.abs(x) 
    end
    if y > 0 then 
        if y == 1 then
            if str == "" then str = "i" else str = str .. "+i" end
        else 
            if str == "" then str = y.."*i" else str = str.."+"..y.."*i" end 
        end
    elseif y < 0 then
        if y == -1 then str = str.."-i" 
        else str = str.."-"..math.abs(y).."*i"
        end
    end
    return str
end
---  surcharge de tostring
function complex.__tostring(z)
    return z:tostring()
end    

--Méthode pour dupliquer ex: z1 = z.dup()
function complex:dup()
    return complex:new(self.re,self.im)
end

-- création simplifiée en cartésien
function complex.Z(x,y)
    return complex:new(x,y)
end

-- création simplifiée en polaire
function complex.Zp(r,theta)
    if (r == nil) or (theta == nil) or ld.notDef(r) or ld.notDef(theta) then return end
    return complex:new(r*math.cos(theta), r*math.sin(theta))
end

function ld.insert(t1,t2,pos)
-- insère les éléments de t2 dans la table t1 (qui est donc modifiée)
-- si pos vaut nil l'insertion se fait à la fin de t1
    local inserer = function(x)
        if pos == nil then table.insert(t1,x) 
        else table.insert(t1,pos,x); pos = pos + 1 
        end
    end
    if (type(t1) ~= "table") then return end
    if type(t2) == "number" then t2 = complex:new(t2,0) end
    if complex.isComplex(t2) then inserer(t2) 
    else
        if type(t2) == "table" then
            for _,x in ipairs(t2) do
                inserer(x)
            end
        else inserer(t2) 
        end
    end
end

function ld.concat(...)
-- concatène les tables passées en arguments et renvoie la table résultante
    local res = {}
    for k,x in ipairs{...} do
        ld.insert(res,x)
    end
    if #res > 0 then return res end
end

function ld.map(f,list)
    local res = {}
    for _, x in ipairs(list) do
        local y = f(x)
        if y == nil then y = complex:new(1e+308,0) end -- on insère jump si résultat nil
        table.insert(res,y)
    end
    return res
end   

function complex.isobar(L)
-- renvoie le centre de gravité de la liste L de complexes
    local x, y, n = 0, 0, 0
    for _,z in ipairs(L) do
        local z1 = complex.toComplex(z)
        if z1 ~= nil then
            x = x+z1.re; y = y+z1.im; n = n+1
        end
    end
    return complex:new(x/n,y/n)
end


function ld.evalf(f,...)
-- cette fonction évalue f(...) et renvoie le résultat si pas d'erreur d'exécution, nil sinon.
    local success, result = pcall(f,...)
    if success then return result end
end


function complex.isListOfCpx(S) -- tests if S is a list of complex numbers
    local ok = function(z)
        return (type(z)=="number") or complex.isComplex(z)
    end
    local ret, k = (S ~= nil) and (type(S)=="table") and (#S>0), 1
    while ret and (k<=#S) do ret = ok(S[k]); k = k+1 end
    return ret
end

function complex.isListOfListOfCpx(S) -- tests if S is a list of lists complex numbers
    local ret, k = (S ~= nil) and (type(S)=="table") and (#S>0), 1
    while ret and (k<=#S) do ret = complex.isListOfCpx(S[k]); k = k+1 end
    return ret
end

local var2string
var2string = function(T)
    if (type(T) ~= "table") or complex.isComplex(T) then return complex.tostring(T)
    else
        return "{ "..table.concat(ld.map(var2string,T),", ").." }"
    end
end

function ld.whatis(x,msg)
    msg = msg or "It"
    msg = "\n"..msg.." "
    if x == nil then print(msg.."is nil")
    elseif type(x) == "number" then  print(msg.."is a number =",x)
    elseif type(x) == "boolean" then print(msg.."is a boolean =",x)
    elseif type(x) == "string" then print(msg.."is a string =", x)
    elseif type(x) == "function" then print(msg.."is a function")
    elseif complex.isComplex(x) then print(msg.."is a complex number =",x)
    elseif complex.isListOfCpx(x) then 
        print(msg.."is a list of numbers/complex numbers\nvalue = "..var2string(x))
    elseif complex.isListOfListOfCpx(x) then 
        print(msg.."is a list of lists of numbers/complex numbers\nvalue = "..var2string(x))
    else
        print(msg.."is a "..type(x).."\n value ="..var2string(x) )
    end
end

complex.I = complex:new(0,1)
complex.Jump = complex:new(1e+308,0)

return complex
