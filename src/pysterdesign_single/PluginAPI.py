from os.path import dirname, basename, isfile, join
import glob

def getPlugins(pluginDirectory: str) -> dict:

    pluginOutput = {}
    modules = glob.glob(join(dirname(__file__), pluginDirectory, "*.py"))
    plugins = [ basename(f)[:-3] for f in modules if isfile(f) and not f.endswith('__init__.py')]
    for plugin in plugins:
        pluginOutput.update({plugin: {"import": __import__('{mod}.{pgn}'.format(mod=pluginDirectory, pgn=plugin), globals(), locals(), ['*'], 0)}})
    return pluginOutput
