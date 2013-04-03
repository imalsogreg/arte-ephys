module LfpViewerState where

import LfpTraceData
import TraceGeometry
import RenderLfp

data SubTrace = SubTrace { traceData   :: TVar TraceData
                         , boundingBox :: BoundingBox
                         , traceOpts   :: TraceOpts
                         }

data ViewerData = ViewerData { renderer   :: LfpTraceData -> IO ()
                             , subTraces  :: [SubTrace]
                             }
                  

