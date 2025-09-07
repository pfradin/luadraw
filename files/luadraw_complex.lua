--- luadraw_complex.lua
-- date 2025/09/07
-- version 2.1
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

require 'luadraw_real'

local complex = {}
complex.__index = complex

--- Constructeur
function complex:new(re, im)
    local cplx = {}
    cplx.re = re
    cplx.im = im
    setmetatable(cplx, self)  -- obligatoire, permet d'utiliser self
    if (re == nil) or (im == nil) or notDef(re) or notDef(im) then return 
    else return cplx
    end
end

-- Fonction pour tester si une variable est de type Complex
-- ATTENTION : la fonction renvoie false pour les réels !
function isComplex(a)
    return getmetatable(a) == complex
end

-- Pour transformer un nombre en complexe si possible
function toComplex(a)
    if isComplex(a) then return a 
    else
        if type(a) == "number" then return Z(a,0) end
    end
end    

-- Méthode pour ajouter, surcharge de +
function complex.__add(u,v)
    u = toComplex(u)
    v = toComplex(v)
    if (u == nil) or (v == nil) then return end
    return complex:new(u.re + v.re, u.im + v.im)
end


--- Méthode pour soustraire deux nombres complexes, surcharge de -
function complex.__sub(u,v)
    u = toComplex(u)
    v = toComplex(v)
    if (u == nil) or (v == nil) then return end
    return complex:new(u.re - v.re, u.im - v.im)
end

--- Méthode pour multiplier deux nombres complexes, surcharge de *
function complex.__mul(u,v)
    u = toComplex(u)
    v = toComplex(v)
    if (u == nil) or (v == nil) then return end
    return complex:new(u.re * v.re - u.im * v.im, u.re * v.im + u.im * v.re)
end

--- Méthode pour diviser deux nombres complexes, surcharge de /
function complex.__div(u,v)
    u = toComplex(u)
    v = toComplex(v)
    if (u == nil) or (v == nil) then return end
    local denominateur = v.re * v.re + v.im * v.im
    if notDef(denominateur) then return
    else
        return complex:new(
        (u.re * v.re + u.im * v.im) / denominateur,
        (u.im * v.re - u.re * v.im) / denominateur)
    end
end

--- Méthode pour tester l'égalité deux nombres complexes, surcharge de =
function complex.__eq(u,v)
    u = toComplex(u)
    v = toComplex(v)
    if (u == nil) or (v == nil) then return end
    return (u.re == v.re) and (u.im == v.im)
end

-- opposé (surcharge de l'opérateur '-c')
function complex.__unm(u)
    u = toComplex(u)
    if (u == nil) then return end    
    return complex:new(-u.re, -u.im)
end

---------------- fonctions mathématiques pour nombres complexes 
--  module
function complex.abs(u)
    if type(u) == "number" then return math.abs(u) end
    u = toComplex(u)
    if (u == nil) then return end
    local x, y = math.abs(u.re), math.abs(u.im)
    if x < y then x, y = y, x end
    if x == 0 then return 0 else return x*math.sqrt(1 + (y/x)^2) end
end

function complex.abs2(u) -- module au carré
    if type(u) == "number" then return math.abs(u) end
    u = toComplex(u)
    if (u == nil) then return end
    return u.re^2 + u.im^2
end

-- argument principal 
function complex.arg(u)
    u = toComplex(u)
    if (u == nil) then return end    
    local x, y = u.re, u.im
    if (x == 0) and (y == 0) then return end
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
    u = toComplex(u)
    v = toComplex(v)
    if (u == nil) or (v == nil) then return end  
    return complex.arg(v/u)
end

-- conjugué 
function complex.bar(u)
    u = toComplex(u)
    if (u == nil) then return end    
    return complex:new(u.re, -u.im)
end

-- exponentielle 
function complex.exp(u)
    u = toComplex(u)
    if (u == nil) then return end    
    local r = math.exp(u.re)
    return complex:new( r*math.cos(u.im), r*math.sin(u.im) )
end

-- arrondir
function complex.round(u, nbDeci)
    u = toComplex(u)
    if (u == nil) then return end    
    return complex:new( round(u.re,nbDeci), round(u.im,nbDeci) )
end

-- norme 1
function complex.N1(u)
    u = toComplex(u)
    if type(u) == "number" then return math.abs(u)
    else return math.abs(u.re) + math.abs(u.im)
    end
end

-- produit scalaire
function complex.dot(u,v)
    u = toComplex(u)
    v = toComplex(v)
    if (u == nil) or (v == nil) then return end    
    local rep = u.re*v.re + u.im*v.im
    if isNul(rep) then return 0 -- nullité à epsilon près
    else return rep
    end
end

-- déterminant    
function complex.det(u,v)
    u = toComplex(u)
    v = toComplex(v)
    if (u == nil) or (v == nil) then return end        
    local rep = u.re*v.im - u.im*v.re
    if isNul(rep) then return 0 -- nullité à epsilon près
    else return rep
    end
end

function complex.isNul(u)
    u = toComplex(u)
    if (u == nil) then return
    else return isNul(u.re) and isNul(u.im)
    end
end    
-------------------------- Méthodes 
function complex:tostring()
    local str = ""
    local x = self.re
    local y = self.im
    
    if notDef(x) or notDef(y) then return "nan" end
    if (x == 0) and (y == 0) then return "0" end
    if x == 1e+308 then return "jump" end
    if x > 0 then str = str..x 
    elseif x < 0 then str = "-"..math.abs(x) 
    end
    if y > 0 then 
        if y == 1 then
            if str == "" then str = "I" else str = str .. "+I" end
        else 
            if str == "" then str = y.."*I" else str = str.."+"..y.."*I" end 
        end
    elseif y < 0 then
        if y == -1 then str = str.."-I" 
        else str = str.."-"..math.abs(y).."*I"
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
function Z(x,y)
    return complex:new(x,y)
end

-- création simplifiée en polaire
function Zp(r,theta)
    if (r == nil) or (theta == nil) or notDef(r) or notDef(theta) then return end
    return complex:new(r*math.cos(theta), r*math.sin(theta))
end

function insert(t1,t2,pos)
-- insère les éléments de t2 dans la table t1 (qui est donc modifiée)
-- si pos vaut nil l'insertion se fait à la fin de t1
    local inserer = function(x)
        if pos == nil then table.insert(t1,x) 
        else table.insert(t1,pos,x); pos = pos + 1 
        end
    end
    if (type(t1) ~= "table") then return end
    if type(t2) == "number" then inserer(Z(t2,0)) 
    else
        if isComplex(t2) then inserer(t2) 
        else
            if type(t2) == "table" then
                for _,x in ipairs(t2) do
                    inserer(x)
                end
            else inserer(t2) 
            end
        end
    end
end

function concat(...)
-- concatène les tables passées en arguments et renvoie la table résultante
    local res = {}
    for k,x in ipairs{...} do
        insert(res,x)
    end
    if #res > 0 then return res end
end

function map(f,list)
    local res = {}
    for _, x in ipairs(list) do
        local y = f(x)
        if y == nil then y = Z(1e+308,0) end -- on insère jump si résultat nil
        table.insert(res,y)
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

return complex
