# -*- coding: utf-8 -*-

project = u'MobilitySimulator'
copyright = u'2022. TU Dortmund University, Institute of Energy Systems, Energy Efficiency and Energy Economics, Research group Distribution grid planning and operation '
version = '1.0'
release = '1.0.0'

# General options
needs_sphinx = '1.0'
master_doc = 'index'
pygments_style = 'tango'
add_function_parentheses = True

extensions = [
    'myst_parser',
    'sphinx.ext.mathjax',
    'sphinx.ext.autosectionlabel',
    'sphinx.ext.autodoc',
    'sphinxcontrib.plantuml',
    'sphinx.ext.intersphinx',
    'hoverxref.extension',
]

# Make sure the target is unique
autosectionlabel_prefix_document = True

myst_enable_extensions = ["dollarmath", "amsmath"]

templates_path = ['_templates']
exclude_trees = ['.build']
source_suffix = ['.md']
source_encoding = 'utf-8-sig'


# HTML options
html_theme = 'sphinx_rtd_theme'
html_short_title = "mobsim"
htmlhelp_basename = 'mobsim-doc'
html_use_index = False
html_show_sourcelink = False
html_static_path = ['_static']

# PlantUML options
plantuml = 'plantuml'