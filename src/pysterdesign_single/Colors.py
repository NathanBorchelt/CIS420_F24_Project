#!/usr/bin/python3.8
#https://stackoverflow.com/a/51592104
from typing import Tuple


def from_rgb(rgb:  Tuple[int, int, int]) -> str:
    """Translates a rgb tuple of int values to hexcodes"""#hexcode
    return "#%02x%02x%02x" % rgb

#the method above, just modified to take 3 values to be a bit easier to type, convers RGB to Hexcode
def from_rgb(r: int, g: int, b: int) -> str:
    """Translates int RGB values to hexcodes"""
    return "#%02x%02x%02x" % (r,g,b)


ColorDictionary = {
    "padx" : 5,
    "pady" : 5,
    'bg' : "#323232",
    "txt_bg" : "#323232",
    "txt_fg" : "#EBEBEB",
    "btn_bg" : "#1E1E1E",
    'btn_fg' : "#EBEBEB",
    'btn_alt_bg' : "#EBEBEB",
    'btn_alt_fg' : "#1E1E1E",

    "fileio_padx" : 5,
    "fileio_pady" : 5,
    'fileio_bg' : "#323232",
    "fileio_txt_bg" : "#323232",
    "fileio_txt_fg" : "#EBEBEB",
    "fileio_btn_bg" : "#1E1E1E",
    'fileio_btn_fg' : "#EBEBEB",
    'fileio_btn_alt_bg' : "#EBEBEB",
    'fileio_btn_alt_fg' : "#1E1E1E",

    "node_padx" : 5,
    "node_pady" : 5,
    "node_bg" : "#3C3C3C",
    "node_txt_bg" : "#3C3C3C",
    "node_txt_fg" : "#EBEBEB",
    "node_ent_bg" : "#1E1E1E",
    "node_ent_fg" : "#EBEBEB",
    "node_btn_bg" : "#444444",
    'node_btn_fg' : "#CCCCCC",
    'node_btn_alt_bg' : "#CCCCCC",
    "node_btn_alt_fg" : "#444444",

    "performance_padx" : 5,
    "performance_pady" : 5,
    "performance_bg" : "#3C3C3C",
    "performance_txt_bg" : "#3C3C3C",
    "performance_txt_fg" : "#EBEBEB",
    "performance_ent_bg" : "#1E1E1E",
    "performance_ent_fg" : "#EBEBEB",
    "performance_btn_bg" : "#444444",
    'performance_btn_fg' : "#CCCCCC",
    'performance_btn_alt_bg' : "#CCCCCC",
    "performance_btn_alt_fg" : "#444444",

    "network_padx" : 5,
    "network_pady" : 5,
    "network_bg" : "#3C3C3C",
    "network_txt_bg" : "#3C3C3C",
    "network_txt_fg" : "#EBEBEB",
    "network_ent_bg" : "#1E1E1E",
    "network_ent_fg" : "#EBEBEB",
    "network_btn_bg" : "#444444",
    'network_btn_fg' : "#CCCCCC",
    'network_btn_alt_bg' : "#CCCCCC",
    "network_btn_alt_fg" : "#444444",

    "ups_padx" : 5,
    "ups_pady" : 5,
    "ups_bg" : "#3C3C3C",
    "ups_txt_bg" : "#3C3C3C",
    "ups_txt_fg" : "#EBEBEB",
    "ups_ent_bg" : "#1E1E1E",
    "ups_ent_fg" : "#EBEBEB",
    "ups_btn_bg" : "#444444",
    'ups_btn_fg' : "#CCCCCC",
    'ups_btn_alt_bg' : "#CCCCCC",
    "ups_btn_alt_fg" : "#444444",

    "design_padx" : 5,
    "design_pady" : 5,
    "design_bg" : "#3C3C3C",
    "design_txt_bg" : "#3C3C3C",
    "design_txt_fg" : "#EBEBEB",
    "design_ent_bg" : "#1E1E1E",
    "design_ent_fg" : "#EBEBEB",
    "design_btn_bg" : "#444444",
    'design_btn_fg' : "#CCCCCC",
    'design_btn_alt_bg' : "#CCCCCC",
    "design_btn_alt_fg" : "#444444",

    "settings_padx" : 5,
    "settings_pady" : 5,
    "settings_bg" : "#3C3C3C",
    "settings_txt_bg" : "#3C3C3C",
    "settings_txt_fg" : "#EBEBEB",
    "settings_ent_bg" : "#1E1E1E",
    "settings_ent_fg" : "#EBEBEB",
    "settings_btn_bg" : "#444444",
    'settings_btn_fg' : "#CCCCCC",
    'settings_btn_alt_bg' : "#CCCCCC",
    "settings_btn_alt_fg" : "#444444",

    "about_padx" : 5,
    "about_pady" : 5,
    "about_bg" : "#3C3C3C",
    "about_txt_bg" : "#3C3C3C",
    "about_txt_fg" : "#EBEBEB",
    "about_ent_bg" : "#1E1E1E",
    "about_ent_fg" : "#EBEBEB",
    "about_btn_bg" : "#444444",
    'about_btn_fg' : "#CCCCCC",
    'about_btn_alt_bg' : "#CCCCCC",
    "about_btn_alt_fg" : "#444444"
}
