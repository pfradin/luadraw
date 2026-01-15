-- luadraw_base.lua (chargé par luadraw_calc.lua)
-- date 2026/01/15
-- version 2.5
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

-- définition des paramètres graphiques
require("luadraw_real")

local luadraw_base = {}
setmetatable(luadraw_base, {__index = luadraw_base}) -- obligatoire pour l'héritage

--- Constructeur
function luadraw_base:new(args)  -- argument de la forme :
-- {window={x1,x2,y1,y2,xscale,yscale}, margin={top, right, bottom, left}, size={large, haut, ratio}, bg="color", border=true/false,bbox=true/false}
    local graph = {}
    setmetatable(graph, {__index = luadraw_base})  -- obligatoire, permet d'utiliser self
    
    args = args or {}
    graph.bg = args.bg or ""
    graph.border = args.border or false
    graph.bbox = (args.bbox == nil) or args.bbox
    local window = args.window or {-5,5,-5,5,1,1}
    local margin = args.margin or {0.5,0.5,0.5,0.5}
    local size = args.size  --size = {largeur, hauteur, ratio}
    local x1,x2,y1,y2,xscale,yscale = table.unpack(window)
    local top,right,bottom,left = table.unpack(margin)
    graph.Xmin = (x1 or -5) ; graph.Xmax = (x2 or 5)  
    graph.Ymin = (y1 or -5) ; graph.Ymax = (y2 or 5)  
    graph.Xscale = (xscale or 1) ; graph.Yscale = (yscale or 1) 
    graph.Lmargin = (left or 0.5) ; graph.Rmargin = (right or 0.5) 
    graph.Tmargin = (top or 0.5) ; graph.Bmargin = (bottom or 0.5) 
    if size ~= nil then -- la taille est imposée
        local large, haut, ratio = table.unpack(size)
        local Ratio = (ratio or (graph.Xscale / graph.Yscale))
        local lg = (large or 0)
        local ht = (haut or lg)
        if lg > 0 then
           graph.Xscale = (lg-graph.Lmargin-graph.Rmargin) / (graph.Xmax-graph.Xmin)
        end
        if ht > 0 then
           graph.Yscale = (ht-graph.Tmargin-graph.Bmargin) / (graph.Ymax-graph.Ymin)
        end
        if Ratio > 0 then
            local aux = Ratio*graph.Yscale
            if aux > graph.Xscale then graph.Yscale = graph.Xscale/Ratio
            else graph.Xscale = aux
            end
        end
    end
    return graph
end

-- méthodes 

function luadraw_base:graphWidth() -- largeur du graph en cm sans les marges
    return (self.Xmax-self.Xmin)*self.Xscale
end    

function luadraw_base:graphHeight() -- hauteur du graph en cm sans les marges
    return (self.Ymax-self.Ymin)*self.Yscale
end


function luadraw_base:coord(x,y) --convertit les coordonnées (x,y) de la fenêtre en coordonnées graphiques
    return (x-self.Xmin)*self.Xscale, (y-self.Ymin)*self.Yscale
end

function luadraw_base:strCoord(x,y) --convertit en chaîne les coordonnées (x,y) de la fenêtre en coordonnées graphiques 
    local u, v = self:coord(x,y)
    return "("..strReal(u)..","..strReal(v)..")"
end

function luadraw_base:clean(L)
-- L est une séquence de complexes, on élimine ceux qui sont trop grands
-- pour tikz et on renvoie une liste de composantes connexes
    local res = {}
    local cp = nil
    local count = 0
    local lastJump = true
    local closeCp = function ()
            if count > 1 then
                table.insert(res, cp)
            end
            lastJump = true
        end
    
    local addCp = function (z)
            if lastJump then 
                cp = {} -- nouvelle composante
                count = 0
                lastJump = false
            end
            table.insert(cp, z)
            count = count + 1
        end
    for _, z in ipairs(L) do
        z = toComplex(z)
        if (z == nil) then closeCp()
        else
            local x, y = self:coord(z.re, z.im)
            if math.abs(x) + math.abs(y) > 1150 then -- trop grand en cm pour tikz
                closeCp()
            else
                addCp(z)
            end
        end
    end
    if (not lastJump) and (count > 0) then table.insert(res, cp) end -- dernière composante
    return res
end    

return luadraw_base
