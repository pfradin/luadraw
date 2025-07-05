--- luadraw_point3d.lua
-- date 2025/07/04
-- version 2.0
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.


local point3d = {}
point3d.__index = point3d

--- Constructeur
function point3d:new(x,y,z)
    local pt3d = {}
    pt3d.x = x
    pt3d.y = y
    pt3d.z = z
    setmetatable(pt3d, self)  -- obligatoire, permet d'utiliser self
    if (x == nil) or (y == nil) or (z == nil) or 
    notDef(x) or notDef(y) or notDef(z) then return 
    else return pt3d
    end
end

-- Fonction pour tester si une variable est de type point3d
function isPoint3d(a)
    return getmetatable(a) == point3d
end

function toPoint3d(a)
-- conversion affixe vers point3d
    if isPoint3d(a) then return a
    elseif isComplex(a) then return point3d:new(a.re,a.im,0)
    elseif type(a) == "number" then return point3d:new(a,0,0)
    end
end


-- Méthode pour ajouter, surcharge de +
function point3d.__add(u,v)
    if (u == nil) or (v == nil) then return end
    return point3d:new(u.x + v.x, u.y + v.y, u.z + v.z)
end


--- Méthode pour soustraire, surcharge de -
function point3d.__sub(u,v)
    if (u == nil) or (v == nil) then return end
    return point3d:new(u.x - v.x, u.y - v.y, u.z - v.z)
end

--- Méthode pour multiplier scalaire et vecteur, surcharge de *
function point3d.__mul(k,u)
    if type(k) ~= "number" then return end
    if (u == nil) then return end
    return point3d:new(k*u.x, k*u.y, k*u.z)
end

--- Méthode pour diviser, surcharge de /
function point3d.__div(u,k)
    if (type(k) ~= "number") or (k == 0) then return end
    if (u == nil) then return end
    return point3d:new(u.x/k, u.y/k, u.z/k)
end

--- Méthode pour tester l'égalité deux nombres complexes, surcharge de =
function point3d.__eq(u,v)
    if (u == nil) or (v == nil) then return end
    return (u.x == v.x) and (u.y == v.y) and (u.z == v.z)
end

-- opposé (surcharge de l'opérateur '-c')
function point3d.__unm(u)
    if (u == nil) then return end    
    return point3d:new(-u.x, -u.y, -u.z)
end

---------------- fonctions mathématiques pour nombres complexes 
--  module
function point3d.abs(u)
    if (u == nil) then return end
    local x, y, z = math.abs(u.x), math.abs(u.y), math.abs(u.z)
    if x < y then x, y = y, x end
    if x < z then x, z = z, x end
    if x == 0 then return 0 else return x*math.sqrt(1 + (y/x)^2 + (z/x)^2) end
end

-- arrondir
function point3d.round(u, nbDeci)
    if (u == nil) then return end    
    return point3d:new( round(u.x,nbDeci), round(u.y,nbDeci), round(u.z,nbDeci) )
end

-- norme 1
function point3d.N1(u)
    if u == nil then return
    else return math.abs(u.x) + math.abs(u.y) + math.abs(u.z)
    end
end

-- produit scalaire
function point3d.dot(u,v)
    if (u == nil) or (v == nil) then return end    
    local rep = u.x*v.x + u.y*v.y + u.z*v.z
    if isNul(rep) then return 0 -- nullité à epsilon près
    else return rep
    end
end

-- déterminant    
function point3d.det(u,v,w)
    if (u == nil) or (v == nil) or (w == nil) then return end        
    local rep = u.x*v.y*w.z + v.x*w.y*u.z + w.x*u.y*v.z - w.x*v.y*u.z - v.x*u.y*w.z - u.x*w.y*v.z
    if isNul(rep) then return 0 -- nullité à epsilon près
    else return rep
    end
end

-- produit vectoriel
function point3d.prod(u,v)
    if (u == nil) or (v == nil) then return end        
    return point3d:new( u.y*v.z-v.y*u.z, u.z*v.x-v.z*u.x, u.x*v.y-v.x*u.y )
end

function point3d.isNul(u)
    if (u == nil) then return
    else return isNul(u.x) and isNul(u.y) and isNul(u.z)
    end
end    

function point3d.normalize(u)
    if not point3d.isNul(u) then
        local r = point3d.abs(u)
        return u/r
    end 
end
-------------------------- Méthodes 
function point3d:tostring()
    return "M("..self.x..","..self.y..","..self.z..")"
end
---  surcharge de tostring
function point3d.__tostring(z)
    return z:tostring()
end    

--Méthode pour dupliquer ex: z1 = z.dup()
function point3d:dup()
    return point3d:new(self.x,self.y,self.z)
end

-- création simplifiée en cartésien
function M(x,y,z)
    return point3d:new(x,y,z)
end

-- création simplifiée en sphérique
function Ms(r,theta,phi)
    if (r == nil) or (theta == nil) or (phi == nil) or notDef(r) or notDef(theta) or notDef(phi) then return end
    return point3d:new(r*math.cos(theta)*math.sin(phi), r*math.sin(theta)*math.sin(phi), r*math.cos(phi))
end

-- création simplifiée en cylindrique
function Mc(r,theta,z)
    if (r == nil) or (theta == nil) or (z == nil) or notDef(r) or notDef(theta) then return end
    return point3d:new(r*math.cos(theta), r*math.sin(theta), z)
end

function angle3d(V1,V2,epsilon)
-- renvoie l'écart angulaire (radians) entre les vecteurs 3d V1 et V2 supposés non nuls
    epsilon = epsilon or 0
    V1 = point3d.normalize(V1); V2 = point3d.normalize(V2)
    if (V1 == nil) or (V2 == nil) then return end
    local alpha = point3d.dot(V1,V2)
    if alpha >= 1-epsilon then 
        return 0 
    elseif alpha <= -1+epsilon then return math.pi 
    else return math.acos(alpha)
    end
end

function isobar3d(L)
-- renvoie le centre de gravité de la liste L de point3d
    local x, y, z, n = 0, 0, 0, 0
    for _,A in ipairs(L) do
        if isPoint3d(A) then
            x = x+A.x; y = y+A.y; z = z+A.z; n = n+1
        end
    end
    return point3d:new(x/n,y/n,z/n)
end

function insert3d(L,A,epsilon)
-- L est une variable représentant une séquence de point3d distincts, A est un point 3d
-- la fonction insère A dans la liste L sans doublon renvoie sa position dans la liste.
-- les comparaisons se font à epsilon près (qui vaut 0 par défaut)
    epsilon = epsilon or 0
    local n = #L
    if n == 0 then table.insert(L,A); return 1 end
    for k,B in ipairs(L) do
        if point3d.N1(B-A) <= epsilon then return k end
    end
    table.insert(L,A)
    return n+1
end

-- constantes
Origin = M(0,0,0)
vecI = M(1,0,0)
vecJ = M(0,1,0)
vecK = M(0,0,1)
ID3d = {Origin, vecI, vecJ, vecK} -- matrice identité 3d

return point3d
