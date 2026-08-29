"""Python interface to the jaspResults C++ core (shared with jaspBase's R engine).

Status: Phase-2 preview — the object classes plus the §3.1/§3.2 table-ingest
conversions from tmp/plan-python-interface.md. The full Analysis runner, state
directory and plotly rendering land in Phase 3.
"""

from jaspresults._jaspresults import *  # noqa: F401,F403
from jaspresults._jaspresults import (  # noqa: F401
	jaspObject,
	jaspHtml,
	jaspReport,
	jaspQmlSource,
	jaspTable,
	jaspPlot,
	jaspState,
	jaspColumn,
	jaspContainer,
	jaspResults,
	NaNString,
	setResponseData,
	setSaveLocation,
	setWriteSealLocation,
	setBaseCitation,
	setDeveloperMode,
	destroyAllAllocatedObjects,
	setSendFunc,
	setLogFunc,
)

__version__ = "0.20.0"
