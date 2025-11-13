-- luadraw_colors.lua (chargé par luadraw_graph)
-- date 2025/11/13
-- version 2.3
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

-- fonctions pour la gestion des couleurs
-- couleurs de base, svgnames du paquet xcolor
Black = {0,0,0}
Blue = {0,0,1}
Brown = {0.6471,0.1647,0.1647}
Cyan = {0,1,1}
DarkGray = {0.6627,0.6627,0.6627}
Gray = {0.502,0.502,0.502}
Green = {0,1,0}
LightGray = {0.8275,0.8275,0.8275}
LightGrey = {0.8275,0.8275,0.8275}
Lime = {0,1,0}
Magenta = {1,0,1}
Olive = {0.502,0.502,0}
Orange = {1,0.6471,0}
Pink = {1,0.7529,0.7961}
Purple = {0.502,0,0.502}
Red = {1,0,0}
Teal = {0,0.502,0.502}
Violet = {0.9333,0.5098,0.9333}
White = {1,1,1}
Yellow = {1,1,0}

-- couleurs avec les noms de svgnames
AliceBlue = {0.9412, 0.9725, 1}
AntiqueWhite = {0.9804, 0.9216, 0.8431}
Aqua = {0.0, 1.0, 1.0}
Aquamarine = {0.498, 1.0, 0.8314}
Azure = {0.9412, 1.0, 1.0}
Beige = {0.9608, 0.9608, 0.8627}
Bisque = {1.0, 0.8941, 0.7686}
BlanchedAlmond = {1.0, 0.9216, 0.8039}
BlueViolet = {0.5412, 0.1686, 0.8863}
Brown = {0.6471, 0.1647, 0.1647}
BurlyWood = {0.8706, 0.7216, 0.5294}
CadetBlue = {0.3725, 0.6196, 0.6275}
Chartreuse = {0.498, 1.0, 0.0}
Chocolate = {0.8235, 0.4118, 0.1176}
Coral = {1.0, 0.498, 0.3137}
CornflowerBlue = {0.3922, 0.5843, 0.9294}
Cornsilk = {1.0, 0.9725, 0.8627}
Crimson = {0.8627, 0.0784, 0.2353}
DarkBlue = {0.0, 0.0, 0.5451}
DarkCyan = {0.0, 0.5451, 0.5451}
DarkGoldenrod = {0.7216, 0.5255, 0.0431}
DarkGray = {0.6627, 0.6627, 0.6627}
DarkGreen = {0.0, 0.3922, 0.0}
DarkKhaki = {0.7412, 0.7176, 0.4196}
DarkMagenta = {0.5451, 0.0, 0.5451}
DarkOliveGreen = {0.3333, 0.4196, 0.1843}
DarkOrange = {1.0, 0.549, 0.0}
DarkOrchid = {0.6, 0.1961, 0.8}
DarkRed = {0.5451, 0.0, 0.0}
DarkSalmon = {0.9137, 0.5882, 0.4784}
DarkSeaGreen = {0.5608, 0.7373, 0.5608}
DarkSlateBlue = {0.2824, 0.2392, 0.5451}
DarkSlateGray = {0.1843, 0.3098, 0.3098}
DarkTurquoise = {0.0, 0.8078, 0.8196}
DarkViolet = {0.5804, 0.0, 0.8275}
DeepPink = {1.0, 0.0784, 0.5765}
DeepSkyBlue = {0.0, 0.749, 1.0}
DimGray = {0.4118, 0.4118, 0.4118}
DodgerBlue = {0.1176, 0.5647, 1.0}
FireBrick = {0.698, 0.1333, 0.1333}
FloralWhite = {1.0, 0.9804, 0.9412}
ForestGreen = {0.1333, 0.5451, 0.1333}
Fuchsia = {1.0, 0.0, 1.0}
Gainsboro = {0.8627, 0.8627, 0.8627}
GhostWhite = {0.9725, 0.9725, 1.0}
Gold = {1.0, 0.8431, 0.0}
Goldenrod = {0.8549, 0.6471, 0.1255}
GreenYellow = {0.6784, 1.0, 0.1843}
Honeydew = {0.9412, 1.0, 0.9412}
HotPink = {1.0, 0.4118, 0.7059}
IndianRed = {0.8039, 0.3608, 0.3608}
Indigo = {0.2941, 0.0, 0.5098}
Ivory = {1.0, 1.0, 0.9412}
Khaki = {0.9412, 0.902, 0.549}
Lavender = {0.902, 0.902, 0.9804}
LavenderBlush = {1.0, 0.9412, 0.9608}
LawnGreen = {0.4863, 0.9882, 0.0}
LemonChiffon = {1.0, 0.9804, 0.8039}
LightBlue = {0.6784, 0.8471, 0.902}
LightCoral = {0.9412, 0.502, 0.502}
LightCyan = {0.8784, 1.0, 1.0}
LightGoldenrodYellow = {0.9804, 0.9804, 0.8235}
LightGray = {0.8275, 0.8275, 0.8275}
LightGreen = {0.5647, 0.9333, 0.5647}
LightPink = {1.0, 0.7137, 0.7569}
LightSalmon = {1.0, 0.6275, 0.4784}
LightSeaGreen = {0.1255, 0.698, 0.6667}
LightSkyBlue = {0.5294, 0.8078, 0.9804}
LightSlateGray = {0.4667, 0.5333, 0.6}
LightSteelBlue = {0.6902, 0.7686, 0.8706}
LightYellow = {1.0, 1.0, 0.8784}
Lime = {0.0, 1.0, 0.0}
LimeGreen = {0.1961, 0.8039, 0.1961}
Linen = {0.9804, 0.9412, 0.902}
Maroon = {0.502, 0.0, 0.0}
MediumAquamarine = {0.4, 0.8039, 0.6667}
MediumBlue = {0.0, 0.0, 0.8039}
MediumOrchid = {0.7294, 0.3333, 0.8275}
MediumPurple = {0.5765, 0.4392, 0.8588}
MediumSeaGreen = {0.2353, 0.702, 0.4431}
MediumSlateBlue = {0.4824, 0.4078, 0.9333}
MediumSpringGreen = {0.0, 0.9804, 0.6039}
MediumTurquoise = {0.2824, 0.8196, 0.8}
MediumVioletRed = {0.7804, 0.0824, 0.5216}
MidnightBlue = {0.098, 0.098, 0.4392}
MintCream = {0.9608, 1.0, 0.9804}
MistyRose = {1.0, 0.8941, 0.8824}
Moccasin = {1.0, 0.8941, 0.7098}
NavajoWhite = {1.0, 0.8706, 0.6784}
Navy = {0.0, 0.0, 0.502}
OldLace = {0.9922, 0.9608, 0.902}
Olive = {0.502, 0.502, 0.0}
OliveDrab = {0.4196, 0.5569, 0.1373}
Orange = {1.0, 0.6471, 0.0}
OrangeRed = {1.0, 0.2706, 0.0}
Orchid = {0.8549, 0.4392, 0.8392}
PaleGoldenrod = {0.9333, 0.9098, 0.6667}
PaleGreen = {0.5961, 0.9843, 0.5961}
PaleTurquoise = {0.6863, 0.9333, 0.9333}
PaleVioletRed = {0.8588, 0.4392, 0.5765}
PapayaWhip = {1.0, 0.9373, 0.8353}
PeachPuff = {1.0, 0.8549, 0.7255}
Peru = {0.8039, 0.5216, 0.2471}
Pink = {1.0, 0.7529, 0.7961}
Plum = {0.8667, 0.6275, 0.8667}
PowderBlue = {0.6902, 0.8784, 0.902}
Purple = {0.502, 0.0, 0.502}
RosyBrown = {0.7373, 0.5608, 0.5608}
RoyalBlue = {0.2549, 0.4118, 0.8824}
SaddleBrown = {0.5451, 0.2706, 0.0745}
Salmon = {0.9804, 0.502, 0.4471}
SandyBrown = {0.9569, 0.6431, 0.3765}
SeaGreen = {0.1804, 0.5451, 0.3412}
Seashell = {1.0, 0.9608, 0.9333}
Sienna = {0.6275, 0.3216, 0.1765}
Silver = {0.7529, 0.7529, 0.7529}
SkyBlue = {0.5294, 0.8078, 0.9216}
SlateBlue = {0.4157, 0.3529, 0.8039}
SlateGray = {0.4392, 0.502, 0.5647}
Snow = {1.0, 0.9804, 0.9804}
SpringGreen = {0.0, 1.0, 0.498}
SteelBlue = {0.2745, 0.5098, 0.7059}
Tan = {0.8235, 0.7059, 0.549}
Teal = {0.0, 0.502, 0.502}
Thistle = {0.8471, 0.749, 0.8471}
Tomato = {1.0, 0.3882, 0.2784}
Turquoise = {0.251, 0.8784, 0.8157}
Violet = {0.9333, 0.5098, 0.9333}
Wheat = {0.9608, 0.8706, 0.702}
WhiteSmoke = {0.9608, 0.9608, 0.9608}
YellowGreen = {0.6039, 0.8039, 0.1961}

function rgb(r,g,b) -- ou rgb({r,g,b})
-- renvoie la couleur dans une chaîne compréhensible par tikz
-- r peut être une table du type {r,g,b} 
-- les composantes r, g et b sont des nombres dans [0,1]
    if type(r) == "table" then
        r, g, b = table.unpack(r)
    end
    return "{rgb,1:red,"..strReal(r)..";green,"..strReal(g)..";blue,"..strReal(b).."}"
end

function hsb(h,s,b,table) 
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
    if table then return res else return rgb(res) end
end

function mixcolor(...) 
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
    return rgb(r/S, g/S, b/S), {r/S,g/S,b/S} -- on renvoie la version chaîne et la version table
end

function palette(colors,pos,tbl)
-- colors est une liste de couleurs au format {r,g,b}
-- pos est un nombre entre 0 et 1 (0=première couleur, 1=dernière couleur)
-- la fonction renvoie la couleur correspondant à la position pos dans la liste colors
-- si tbl vaut true, on renvoie une table, sinon une chaîne
    local N = #colors
    tbl = tbl or false
    if pos == 0 then if tbl then return colors[1] else return rgb(colors[1]) end
    elseif pos == 1 then if tbl then return colors[N] else return rgb(colors[N]) end
    else
        local x = pos*(N-1)+1
        local k = math.floor(x)
        local p = x-k
        local res1, res2 = mixcolor(colors[k],1-p,colors[k+1],p)
        if tbl then return res2
        else return res1
        end
    end
end

function getpalette(colors,nb,tbl)
-- colors est une liste de couleurs au format {r,g,b} (tables)
-- nb est le nombre de couleurs souhaitées
-- la fonction renvoie une liste de nb couleurs régulièrement réparties dans colors
-- si tbl vaut true, on renvoie une liste de tables, sinon une liste de chaînes
    local N = #colors
    tbl = tbl or false
    local res = {}
    for k = 0, nb-1 do
        table.insert(res, palette(colors,k/(nb-1),tbl))
    end
    return res
end

-- Quelques palettes
rainbow = {Purple,Indigo,Blue,Green,Yellow,Orange,Red}

palGasFlame = {
    {0.8784, 1, 1},
    {0.5294, 0.8078, 0.9804},
    {0.8667, 0.6275, 0.8667},
    {0.9412, 0.502, 0.502},
    {1, 0.6275, 0.4784},
    {0.9412, 0.902, 0.549}
}

palRainbow = {
    {0.5, 0.0, 1.0},
    {0.21764, 0.42912, 0.97551},
    {0.07254, 0.78292, 0.90058},
    {0.3549, 0.97413, 0.78292},
    {0.64509, 0.97413, 0.62211},
    {0.92745, 0.78292, 0.43467},
    {1.0, 0.42912, 0.21994},
    {1.0, 0.0, 0.0}
}

palAutumn = {
    {1.0, 0.0, 0.0},
    {1.0, 0.14117, 0.0},
    {1.0, 0.28627, 0.0},
    {1.0, 0.42745, 0.0},
    {1.0, 0.57254, 0.0},
    {1.0, 0.71372, 0.0},
    {1.0, 0.85882, 0.0},
    {1.0, 1.0, 0.0}
}

palGistGray = {
    {0.0, 0.0, 0.0},
    {0.14117, 0.14117, 0.14117},
    {0.28627, 0.28627, 0.28627},
    {0.42745, 0.42745, 0.42745},
    {0.57254, 0.57254, 0.57254},
    {0.71372, 0.71372, 0.71372},
    {0.85882, 0.85882, 0.85882},
    {1.0, 1.0, 1.0}
}
