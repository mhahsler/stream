#include "SHC_R.cpp"
#include <RcppEigen.h>
using namespace Rcpp;
using namespace std;

RCPP_EXPOSED_AS(SHC_R)
RCPP_MODULE(SHCModule) {
    Rcpp::class_<SHC_R>("SHC_R")
    .constructor<int,int,int,int,int>()
    .constructor<List>()
    .constructor<SHC_R>()
    .method("process",&SHC_R::processForR,"SHC processing entry")
    .method("getClusters",&SHC_R::getClusters,"Get containers that contain components and outliers")
    .method("getComponents",&SHC_R::getComponents,"Get components for the supplied cluster")
    .method("getClusterContours",&SHC_R::getClusterContours,"Get cluster component(s) classification bounds")
    .method("cleanOutliers",&SHC_R::cleanOutliers,"Remove outliers from SHC models")
    .method("getComponentDetails",&SHC_R::getComponentDetails,"Get component details")
    .method("theta",&SHC_R::getTheta,"Get the SHC classifier statistical threshold")
    .method("virtualVariance",&SHC_R::getVirtualVariance,"Get the SHC classifier statistical virtual variance")
    .method("isTraceable",&SHC_R::isTraceable,"Are two components traceable")
    .method("getAllComponents",&SHC_R::getAllComponents,"Get components for the supplied cluster")
    .method("getClusterWeight",&SHC_R::getClusterWeight,"Get cluster weight")
    .method("getComponentWeight",&SHC_R::getComponentWeight,"Get component weight")
    .method("getClusterMapping",&SHC_R::getClusterMapping,"Get cluster mapping")
    .method("getComponentMapping",&SHC_R::getComponentMapping,"Get component mapping")
    .method("microToMacro",&SHC_R::stream_MicroToMacro,"Micro to macro mapping for the stream package")
    .method("microToMicro",&SHC_R::stream_MicroToMicro,"Micro to micro mapping for the stream package")
    .method("stats",&SHC_R::getStats,"Processing stats")
    .method("useSigmaIndex",&SHC_R::useSigmaIndex,"Use sigma-indexing tree")
    .method("setPseudoOfflineCounter",&SHC_R::setPseudoOfflineCounter,"Set pseudo-offline counter")
    .method("getTimes",&SHC_R::getTimes,"Get variety of execution timings")
    .method("getNodeCounter",&SHC_R::getNodeCounter,"Get node counter")
    .method("getComputationCostReduction",&SHC_R::getSigmaIndexR,"Get computation cost reduction")
    .method("getHistogram",&SHC_R::getSigmaIndexHistogram,"Get histogram")
    .method("recheckOutlier",&SHC_R::recheckOutlier,"Recheck outlier")
    .method("getOutlierPositions",&SHC_R::getOutlierPositions,"Get outlier positions")
    .method("getTrace",&SHC_R::getTrace,"Get component or outlier trace")
    .method("cloneSHC",&SHC_R::cloneSHC,"SHC cloning interface")
    .method("clearEigenMPSupport",&SHC_R::clearEigenMPSupport,"Clear Eigen MP support");
}