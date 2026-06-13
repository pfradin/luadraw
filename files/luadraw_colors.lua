-- luadraw_colors.lua (chargé par luadraw_graph)
-- date 2026/06/13
-- version 3.2
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl

-- fonctions pour la gestion des couleurs
-- couleurs de base, svgnames du paquet xcolor

local ld = luadraw
local strReal = ld.strReal


ld.Black = {0,0,0}
ld.Blue = {0,0,1}
ld.Brown = {0.6471,0.1647,0.1647}
ld.Cyan = {0,1,1}
ld.DarkGray = {0.6627,0.6627,0.6627}
ld.Gray = {0.502,0.502,0.502}
ld.Green = {0,1,0}
ld.LightGray = {0.8275,0.8275,0.8275}
ld.LightGrey = {0.8275,0.8275,0.8275}
ld.Lime = {0,1,0}
ld.Magenta = {1,0,1}
ld.Olive = {0.502,0.502,0}
ld.Orange = {1,0.6471,0}
ld.Pink = {1,0.7529,0.7961}
ld.Purple = {0.502,0,0.502}
ld.Red = {1,0,0}
ld.Teal = {0,0.502,0.502}
ld.Violet = {0.9333,0.5098,0.9333}
ld.White = {1,1,1}
ld.Yellow = {1,1,0}

-- couleurs avec les noms de svgnames
ld.AliceBlue = {0.9412, 0.9725, 1}
ld.AntiqueWhite = {0.9804, 0.9216, 0.8431}
ld.Aqua = {0.0, 1.0, 1.0}
ld.Aquamarine = {0.498, 1.0, 0.8314}
ld.Azure = {0.9412, 1.0, 1.0}
ld.Beige = {0.9608, 0.9608, 0.8627}
ld.Bisque = {1.0, 0.8941, 0.7686}
ld.BlanchedAlmond = {1.0, 0.9216, 0.8039}
ld.BlueViolet = {0.5412, 0.1686, 0.8863}
ld.Brown = {0.6471, 0.1647, 0.1647}
ld.BurlyWood = {0.8706, 0.7216, 0.5294}
ld.CadetBlue = {0.3725, 0.6196, 0.6275}
ld.Chartreuse = {0.498, 1.0, 0.0}
ld.Chocolate = {0.8235, 0.4118, 0.1176}
ld.Coral = {1.0, 0.498, 0.3137}
ld.CornflowerBlue = {0.3922, 0.5843, 0.9294}
ld.Cornsilk = {1.0, 0.9725, 0.8627}
ld.Crimson = {0.8627, 0.0784, 0.2353}
ld.DarkBlue = {0.0, 0.0, 0.5451}
ld.DarkCyan = {0.0, 0.5451, 0.5451}
ld.DarkGoldenrod = {0.7216, 0.5255, 0.0431}
ld.DarkGray = {0.6627, 0.6627, 0.6627}
ld.DarkGreen = {0.0, 0.3922, 0.0}
ld.DarkKhaki = {0.7412, 0.7176, 0.4196}
ld.DarkMagenta = {0.5451, 0.0, 0.5451}
ld.DarkOliveGreen = {0.3333, 0.4196, 0.1843}
ld.DarkOrange = {1.0, 0.549, 0.0}
ld.DarkOrchid = {0.6, 0.1961, 0.8}
ld.DarkRed = {0.5451, 0.0, 0.0}
ld.DarkSalmon = {0.9137, 0.5882, 0.4784}
ld.DarkSeaGreen = {0.5608, 0.7373, 0.5608}
ld.DarkSlateBlue = {0.2824, 0.2392, 0.5451}
ld.DarkSlateGray = {0.1843, 0.3098, 0.3098}
ld.DarkTurquoise = {0.0, 0.8078, 0.8196}
ld.DarkViolet = {0.5804, 0.0, 0.8275}
ld.DeepPink = {1.0, 0.0784, 0.5765}
ld.DeepSkyBlue = {0.0, 0.749, 1.0}
ld.DimGray = {0.4118, 0.4118, 0.4118}
ld.DodgerBlue = {0.1176, 0.5647, 1.0}
ld.FireBrick = {0.698, 0.1333, 0.1333}
ld.FloralWhite = {1.0, 0.9804, 0.9412}
ld.ForestGreen = {0.1333, 0.5451, 0.1333}
ld.Fuchsia = {1.0, 0.0, 1.0}
ld.Gainsboro = {0.8627, 0.8627, 0.8627}
ld.GhostWhite = {0.9725, 0.9725, 1.0}
ld.Gold = {1.0, 0.8431, 0.0}
ld.Goldenrod = {0.8549, 0.6471, 0.1255}
ld.GreenYellow = {0.6784, 1.0, 0.1843}
ld.Honeydew = {0.9412, 1.0, 0.9412}
ld.HotPink = {1.0, 0.4118, 0.7059}
ld.IndianRed = {0.8039, 0.3608, 0.3608}
ld.Indigo = {0.2941, 0.0, 0.5098}
ld.Ivory = {1.0, 1.0, 0.9412}
ld.Khaki = {0.9412, 0.902, 0.549}
ld.Lavender = {0.902, 0.902, 0.9804}
ld.LavenderBlush = {1.0, 0.9412, 0.9608}
ld.LawnGreen = {0.4863, 0.9882, 0.0}
ld.LemonChiffon = {1.0, 0.9804, 0.8039}
ld.LightBlue = {0.6784, 0.8471, 0.902}
ld.LightCoral = {0.9412, 0.502, 0.502}
ld.LightCyan = {0.8784, 1.0, 1.0}
ld.LightGoldenrodYellow = {0.9804, 0.9804, 0.8235}
ld.LightGray = {0.8275, 0.8275, 0.8275}
ld.LightGreen = {0.5647, 0.9333, 0.5647}
ld.LightPink = {1.0, 0.7137, 0.7569}
ld.LightSalmon = {1.0, 0.6275, 0.4784}
ld.LightSeaGreen = {0.1255, 0.698, 0.6667}
ld.LightSkyBlue = {0.5294, 0.8078, 0.9804}
ld.LightSlateGray = {0.4667, 0.5333, 0.6}
ld.LightSteelBlue = {0.6902, 0.7686, 0.8706}
ld.LightYellow = {1.0, 1.0, 0.8784}
ld.Lime = {0.0, 1.0, 0.0}
ld.LimeGreen = {0.1961, 0.8039, 0.1961}
ld.Linen = {0.9804, 0.9412, 0.902}
ld.Maroon = {0.502, 0.0, 0.0}
ld.MediumAquamarine = {0.4, 0.8039, 0.6667}
ld.MediumBlue = {0.0, 0.0, 0.8039}
ld.MediumOrchid = {0.7294, 0.3333, 0.8275}
ld.MediumPurple = {0.5765, 0.4392, 0.8588}
ld.MediumSeaGreen = {0.2353, 0.702, 0.4431}
ld.MediumSlateBlue = {0.4824, 0.4078, 0.9333}
ld.MediumSpringGreen = {0.0, 0.9804, 0.6039}
ld.MediumTurquoise = {0.2824, 0.8196, 0.8}
ld.MediumVioletRed = {0.7804, 0.0824, 0.5216}
ld.MidnightBlue = {0.098, 0.098, 0.4392}
ld.MintCream = {0.9608, 1.0, 0.9804}
ld.MistyRose = {1.0, 0.8941, 0.8824}
ld.Moccasin = {1.0, 0.8941, 0.7098}
ld.NavajoWhite = {1.0, 0.8706, 0.6784}
ld.Navy = {0.0, 0.0, 0.502}
ld.OldLace = {0.9922, 0.9608, 0.902}
ld.Olive = {0.502, 0.502, 0.0}
ld.OliveDrab = {0.4196, 0.5569, 0.1373}
ld.Orange = {1.0, 0.6471, 0.0}
ld.OrangeRed = {1.0, 0.2706, 0.0}
ld.Orchid = {0.8549, 0.4392, 0.8392}
ld.PaleGoldenrod = {0.9333, 0.9098, 0.6667}
ld.PaleGreen = {0.5961, 0.9843, 0.5961}
ld.PaleTurquoise = {0.6863, 0.9333, 0.9333}
ld.PaleVioletRed = {0.8588, 0.4392, 0.5765}
ld.PapayaWhip = {1.0, 0.9373, 0.8353}
ld.PeachPuff = {1.0, 0.8549, 0.7255}
ld.Peru = {0.8039, 0.5216, 0.2471}
ld.Pink = {1.0, 0.7529, 0.7961}
ld.Plum = {0.8667, 0.6275, 0.8667}
ld.PowderBlue = {0.6902, 0.8784, 0.902}
ld.Purple = {0.502, 0.0, 0.502}
ld.RosyBrown = {0.7373, 0.5608, 0.5608}
ld.RoyalBlue = {0.2549, 0.4118, 0.8824}
ld.SaddleBrown = {0.5451, 0.2706, 0.0745}
ld.Salmon = {0.9804, 0.502, 0.4471}
ld.SandyBrown = {0.9569, 0.6431, 0.3765}
ld.SeaGreen = {0.1804, 0.5451, 0.3412}
ld.Seashell = {1.0, 0.9608, 0.9333}
ld.Sienna = {0.6275, 0.3216, 0.1765}
ld.Silver = {0.7529, 0.7529, 0.7529}
ld.SkyBlue = {0.5294, 0.8078, 0.9216}
ld.SlateBlue = {0.4157, 0.3529, 0.8039}
ld.SlateGray = {0.4392, 0.502, 0.5647}
ld.Snow = {1.0, 0.9804, 0.9804}
ld.SpringGreen = {0.0, 1.0, 0.498}
ld.SteelBlue = {0.2745, 0.5098, 0.7059}
ld.Tan = {0.8235, 0.7059, 0.549}
ld.Teal = {0.0, 0.502, 0.502}
ld.Thistle = {0.8471, 0.749, 0.8471}
ld.Tomato = {1.0, 0.3882, 0.2784}
ld.Turquoise = {0.251, 0.8784, 0.8157}
ld.Violet = {0.9333, 0.5098, 0.9333}
ld.Wheat = {0.9608, 0.8706, 0.702}
ld.WhiteSmoke = {0.9608, 0.9608, 0.9608}
ld.YellowGreen = {0.6039, 0.8039, 0.1961}

function ld.rgb(r,g,b) -- ou rgb({r,g,b})
-- renvoie la couleur dans une chaîne compréhensible par tikz
-- r peut être une table du type {r,g,b} 
-- les composantes r, g et b sont des nombres dans [0,1]
    if type(r) == "table" then
        r, g, b = table.unpack(r)
    end
    return "{rgb,1:red,"..strReal(r)..";green,"..strReal(g)..";blue,"..strReal(b).."}"
end

function ld.hsb(h,s,b,table) 
-- hsb(hue (0..360), saturation (0..1), brightness (0..1) ) 
-- conversion vers rgb
-- renvoie une chaîne pour tikz ou une table
    table = table or false
    local Hi = math.floor(h/60) % 6
    local f = h/60-Hi
    local p = b*(1-s) 
    local q = b*(1-f*s)
    local t = b*(1-(1-f)*s)
    local res
    if Hi == 0 then res = {b,t,p}
    elseif Hi == 1 then res = {q,b,p}
    elseif Hi == 2 then res = {p,b,t}
    elseif Hi == 3 then res = {p,q,b}
    elseif Hi == 4 then res = {t,p,b}
    else res = {b,p,q}
    end
    if table then return res else return ld.rgb(res) end
end

function ld.mixcolor(...) 
--mixcolor( color1, proportion1, color2, proportion2, ..., colorN, proportionN): mélange les couleurs suivants les proportions
-- chaque couleur est une table {r,g,b}
-- renvoie une chaîne rgb pour tikz et une table
    local S, r, g, b, a1, a2, a3 = 0, 0, 0, 0
    for k,coef in ipairs{...} do
        if k%2 == 1 then -- k impair c'est une couleur
            a1, a2, a3 = table.unpack(coef)
        else -- k est pair c'est un coefficient
            r = r+a1*coef; g = g+a2*coef; b = b+a3*coef
            S = S+coef
        end
    end
    return ld.rgb(r/S, g/S, b/S), {r/S,g/S,b/S} -- on renvoie la version chaîne et la version table
end

function ld.mixpalette(pal, percent, color)
-- pal = palette of colors (rgb tables)
-- percent in [0;100]
-- color = rgb table
    color = color or White
    local mix = {}
    for _,c in ipairs(pal) do
        local str, tbl =  ld.mixcolor(c,percent,color,100-percent) -- str = string format, tbl = table format
        table.insert(mix, tbl)
    end
    return mix
end

function ld.palette(colors,pos,tbl)
-- colors est une liste de couleurs au format {r,g,b}
-- pos est un nombre entre 0 et 1 (0=première couleur, 1=dernière couleur)
-- la fonction renvoie la couleur correspondant à la position pos dans la liste colors
-- si tbl vaut true, on renvoie une table, sinon une chaîne
    local N = #colors
    tbl = tbl or false
    if pos == 0 then if tbl then return colors[1] else return ld.rgb(colors[1]) end
    elseif pos == 1 then if tbl then return colors[N] else return ld.rgb(colors[N]) end
    else
        local x = pos*(N-1)+1
        local k = math.floor(x)
        local p = x-k
        local res1, res2 = ld.mixcolor(colors[k],1-p,colors[k+1],p)
        if tbl then return res2
        else return res1
        end
    end
end

function ld.getpalette(colors,nb,tbl)
-- colors est une liste de couleurs au format {r,g,b} (tables)
-- nb est le nombre de couleurs souhaitées
-- la fonction renvoie une liste de nb couleurs régulièrement réparties dans colors
-- si tbl vaut true, on renvoie une liste de tables, sinon une liste de chaînes
    local N = #colors
    tbl = tbl or false
    local res = {}
    for k = 0, nb-1 do
        table.insert(res, ld.palette(colors,k/(nb-1),tbl))
    end
    return res
end

-- Quelques palettes
ld.rainbow = {ld.Purple,ld.Indigo,ld.Blue,ld.Green,ld.Yellow,ld.Orange,ld.Red}

ld.palGasFlame = {
    {0.8784, 1, 1},
    {0.5294, 0.8078, 0.9804},
    {0.8667, 0.6275, 0.8667},
    {0.9412, 0.502, 0.502},
    {1, 0.6275, 0.4784},
    {0.9412, 0.902, 0.549}
}

ld.palRainbow = {
    {0.5, 0.0, 1.0},
    {0.21764, 0.42912, 0.97551},
    {0.07254, 0.78292, 0.90058},
    {0.3549, 0.97413, 0.78292},
    {0.64509, 0.97413, 0.62211},
    {0.92745, 0.78292, 0.43467},
    {1.0, 0.42912, 0.21994},
    {1.0, 0.0, 0.0}
}

ld.palAutumn = {
    {1.0, 0.0, 0.0},
    {1.0, 0.14117, 0.0},
    {1.0, 0.28627, 0.0},
    {1.0, 0.42745, 0.0},
    {1.0, 0.57254, 0.0},
    {1.0, 0.71372, 0.0},
    {1.0, 0.85882, 0.0},
    {1.0, 1.0, 0.0}
}

ld.palGistGray = {
    {0.0, 0.0, 0.0},
    {0.14117, 0.14117, 0.14117},
    {0.28627, 0.28627, 0.28627},
    {0.42745, 0.42745, 0.42745},
    {0.57254, 0.57254, 0.57254},
    {0.71372, 0.71372, 0.71372},
    {0.85882, 0.85882, 0.85882},
    {1.0, 1.0, 1.0}
}
