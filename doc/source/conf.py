# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'imctk'
copyright = '2024, YosysHQ'
author = 'YosysHQ'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = ["sphinxcontrib_rust", "myst_parser"]
source_suffix = {
    ".rst": "restructuredtext",
    ".md": "markdown",
    ".txt": "markdown", # Optional
}
myst_enable_extensions = {
    "colon_fence",
    "html_admonition",
    "replacements",
    "smartquotes",
    "strikethrough",
    "tasklist",
}
rust_crates = {
    "ids": "../ids",
    "imctk-derive": "../derive",
    "imctk-transparent": "../transparent",
    "table_seq": "../table_seq",
    "imctk-aiger": "../aiger",
    "imctk-logger": "../logger",
    "imctk-util": "../util",
    "imctk-ir": "../ir",
    "imctk-abc-sys": "../abc-sys",
    "imctk-abc": "../abc",
    "imctk-eqy-engine": "../eqy-engine",
    "imctk-inc_refine": "../inc_refine",
    "imctk-extract": "../extract",
    "imctk": "../imctk",
    "imctk-lit": "../lit",
}
rust_doc_dir = "build/crates/"
rust_rustdoc_fmt = "rst"


templates_path = ['_templates']
exclude_patterns = []



# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'alabaster'
html_static_path = ['_static']
