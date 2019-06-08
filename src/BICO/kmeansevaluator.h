#ifndef KMEANSEVALUATOR_H
#define KMEANSEVALUATOR_H

#include "measuresetter.h"
#include "partitionprovider.h"
#include "proxyevaluation.h"
#include "combinedevaluation.h"
#include "dissimilaritymeasure.h"

namespace CluE
{

//TODO check correct use of either "std::vector<T> proxies" or "std::vector<T*> proxies", use combinedcost in proxycost(?)

/**
 * @brief Calculates the k-means weight.
 * @note Arguments named \e firstSecondThird may be shortened \e fST in formulas.
 * @note The distance measure is referred to as \e d(x,y) in formulas.
 * @ingroup evaluation_algorithms
 */
template<typename T> class KMeansEvaluator : public ProxyEvaluation<T>, public CombinedEvaluation<T>, public MeasureSetter<T>
{
public:

	/**
	 * @brief Instantiates KMeansEvaluator, optionally with a DissimilarityMeasure to use when calculation the maximum diameter.
	 * @param measure Optional. Nevertheless, you have to set a DissimilarityMeasure before using this class.
	 * @see setMeasure
	 */
	KMeansEvaluator(DissimilarityMeasure<T> const *measure = 0);

	KMeansEvaluator(const KMeansEvaluator<T>&);
	KMeansEvaluator<T>& operator= (const KMeansEvaluator<T>&);
	virtual ~KMeansEvaluator();

	/**
	 * @brief Sets the DissimilarityMeasure used when calculating the maximum diameter.
	 */
	virtual void setMeasure(DissimilarityMeasure<T> const *measure);

	void setWeightModifier(WeightModifier<T>* wm);

//# ProxyEvaluation members
	/**
	 * @brief Assigns all points to a proxy and calculates the k-means weight of the resulting clustering.
	 * @return \f[ \sum{ \{d(p, \mathrm{proxy}(x)) \: | \: p \in \mathtt{points} \}} \f]
	 *         where \f$ \mathrm{proxy}(x) \f$ assigns the nearest proxy from proxies to x.
	 */
	virtual double proxycost(std::vector<T*> const& points, std::vector<T> const& proxies) const;

	/**
	 * @overload
	 */
	virtual double proxycost(std::vector<T*> const& points, std::vector<T*> const& proxies) const;

	/**
	 * @brief Assigns all points to a proxy (provided by proxySource) and calculates the k-means weight of the resulting clustering.
	 * @return \f[ \sum{ \{d(p, \mathrm{proxy}(x)) \: | \: p \in \mathtt{points} \}} \f]
	 *         where \f$ \mathrm{proxy}(x) \f$ assigns the nearest proxy from \f$ \mathtt{pS}_\mathtt{sI} \f$ to x.
	 */
	virtual double proxycost(std::vector<T*> const& points, ProxyProvider<T> const &proxySource, unsigned int solutionIndex) const;

	/**
	 * @overload
	 */
	virtual double proxycost(std::vector<T*> const& points, DiscreteProxyProvider<T> const &proxySource, unsigned int solutionIndex) const;


	/**
	 * @brief Assigns all points to a proxy and calculates the k-means weight of the cluster[index].
	 * @return \f[ \sum{ \{d(p, \mathrm{proxy}(x)) \: | \: p \in \mathtt{points}, \mathrm{proxy}(x)=\mathtt{proxies}_\mathtt{index} \} } \f]
	 *         where \f$ \mathrm{proxy}(x) \f$ assigns the nearest proxy from proxies to x.
	 */
	virtual double proxycost(std::vector<T*> const& points, std::vector<T> const& proxies, unsigned int index) const;

	/**
	 * @overload
	 */
	virtual double proxycost(std::vector<T*> const& points, std::vector<T*> const& proxies, unsigned int index) const;

	/**
	 * @brief Assigns all points to a proxy (provided by proxySource) and calculates the k-means weight of the cluster[index].
	 * @return \f[ \sum{ \{d(p, \mathrm{proxy}(x)) \: | \: p \in \mathtt{points}, \mathrm{proxy}(x)=\mathtt{pS}_{\mathtt{sI}_\mathtt{pI}} \} } \f]
	 *         where \f$ \mathrm{proxy}(x) \f$ assigns the nearest proxy from \f$ \mathtt{pS}_\mathtt{sI} \f$ to x.
	 */
	virtual double proxycost(std::vector<T*> const& points, ProxyProvider<T> const &proxySource, unsigned int solutionIndex, unsigned int proxyIndex) const;

	/**
	 * @overload
	 */
	virtual double proxycost(std::vector<T*> const& points, DiscreteProxyProvider<T> const &proxySource, unsigned int solutionIndex, unsigned int proxyIndex) const;

//# CombinedEvaluation members
	/**
	 * @brief Calculates the k-means weight of a given clustering.
	 * @return \f[ \sum{ \{ d(p_{i,j}, \mathtt{proxies}_i) \: | \: p_{i,j} \in \mathtt{clusters}_i \} } \f]
	 */
	virtual double combinedcost(std::vector<std::vector<T*> > const& clusters, std::vector<T> const& proxies) const;

	/**
	 * @overload
	 */
	virtual double combinedcost(std::vector<std::vector<T*> > const& clusters, std::vector<T*> const& proxies) const;


	/**
	 * @brief Calculates the k-means weight of a given clustering (provided by clusteringSource and proxySource).
	 * @return \f[ \sum{ \{ d(p_{i,j}, \mathtt{pS}_{\mathtt{sI}_i}) \: | \: p_{i,j} \in \mathtt{cS}_{\mathtt{sI}_i} \} } \f]
	 */
	virtual double combinedcost(PartitionProvider<T> const &clusteringSource, ProxyProvider<T> const &proxySource, unsigned int solutionIndex) const;

	/**
	 * @overload
	 */
	virtual double combinedcost(PartitionProvider<T> const &clusteringSource, DiscreteProxyProvider<T> const &proxySource, unsigned int solutionIndex) const;


	/**
	 * @brief Calculates the k-means weight of a given proxy and the corresponding points.
	 * @return \f[ \sum{ \{ d(p, \mathtt{proxy}) \: | \: p \in \mathtt{cluster} \} } \f]
	 */
	virtual double combinedcost(std::vector<T*> const& cluster, T const& proxy) const;

	/**
	 * @brief Calculates the k-means weight of a given proxy (provided by proxySource) and the corresponding points (provided by clusteringSource).
	 * @return \f[ \sum{ \{ d(p, \mathtt{pS}_{\mathtt{sI}_\mathtt{pI}}) \: | \: p \in \mathtt{cS}_{\mathtt{sI}_\mathtt{pI}} \} } \f]
	 */
	virtual double combinedcost(PartitionProvider<T> const &clusteringSource, ProxyProvider<T> const &proxySource, unsigned int solutionIndex, unsigned int proxyIndex) const;

	/**
	 * @overload
	 */
	virtual double combinedcost(PartitionProvider<T> const &clusteringSource, DiscreteProxyProvider<T> const &proxySource, unsigned int solutionIndex, unsigned int proxyIndex) const;

protected:
	/**
	 * @brief Provides a k-means weight result per cluster (may be added, chosen from, ...).
	 */
	std::vector<double> proxycostGeneric(std::vector<T*> const& points, std::vector<T> const& proxies) const;

	/**
	 * @overload
	 */
	std::vector<double> proxycostGeneric(std::vector<T*> const& points, std::vector<T*> const& proxies) const;

private:
	DissimilarityMeasure<T>* measure;
	WeightModifier<T>* weightModifier;
};

template<typename T> KMeansEvaluator<T>::KMeansEvaluator(DissimilarityMeasure<T> const *measure) :
	measure(measure==0 ? 0 : measure->clone()), weightModifier(0)
{
	//empty
}

template<typename T> KMeansEvaluator<T>::KMeansEvaluator(const KMeansEvaluator<T>& kme) :
	measure(kme.measure == 0 ? 0 : kme.measure->clone()), weightModifier(0)
{
}

template<typename T> KMeansEvaluator<T>& KMeansEvaluator<T>::operator= (const KMeansEvaluator<T>& kme)
{
	if(measure != 0)
		delete measure;
	if(weightModifier != 0)
		delete weightModifier;

	ProxyEvaluation<T>::operator= (kme);
	CombinedEvaluation<T>::operator= (kme);

	measure = kme.measure == 0 ? 0 : kme.measure->clone();
	weightModifier = kme.weightModifier == 0 ? 0 : kme.weightModifier->clone();

	return *this;
}

template<typename T> KMeansEvaluator<T>::~KMeansEvaluator()
{
	if(measure != 0)
		delete measure;
	if(weightModifier != 0)
		delete weightModifier;
}

//### ProxyEvaluation members

template<typename T> double KMeansEvaluator<T>::proxycost(std::vector<T*> const& points, std::vector<T> const& proxies) const
{
	std::vector<double> values = proxycostGeneric(points, proxies);
	int numOfValues = values.size();

	double result = 0;
	for(int i = 0; i < numOfValues; i++)
	{
		result += values[i];
	}

	return result;
}

template<typename T> double KMeansEvaluator<T>::proxycost(std::vector<T*> const& points, std::vector<T*> const& proxies) const
{
	std::vector<double> values = proxycostGeneric(points, proxies);
	int numOfValues = values.size();

	double result = 0;
	for(int i = 0; i < numOfValues; i++)
	{
		result += values[i];
	}

	return result;
}

template<typename T> double KMeansEvaluator<T>::proxycost(std::vector<T*> const& points, ProxyProvider<T> const &provider, unsigned int solutionIndex) const
{
	return proxycost(points, provider.proxies(solutionIndex));
}

template<typename T> double KMeansEvaluator<T>::proxycost(std::vector<T*> const& points, DiscreteProxyProvider<T> const &provider, unsigned int solutionIndex) const
{
	return proxycost(points, provider.discrete_proxies(solutionIndex));
}


template<typename T> double KMeansEvaluator<T>::proxycost(std::vector<T*> const& points, std::vector<T> const& proxies, unsigned int index) const
{
	std::vector<double> values = proxycostGeneric(points, proxies);
	return values.at(index);
}

template<typename T> double KMeansEvaluator<T>::proxycost(std::vector<T*> const& points, std::vector<T*> const& proxies, unsigned int index) const
{
	std::vector<double> values = proxycostGeneric(points, proxies);
	return values.at(index);
}

template<typename T> double KMeansEvaluator<T>::proxycost(std::vector<T*> const& points, ProxyProvider<T> const &provider, unsigned int solutionIndex, unsigned int proxyIndex) const
{
	return proxycost(points, provider.proxies(solutionIndex), proxyIndex);
}

template<typename T> double KMeansEvaluator<T>::proxycost(std::vector<T*> const& points, DiscreteProxyProvider<T> const &provider, unsigned int solutionIndex, unsigned int proxyIndex) const
{
	return proxycost(points, provider.discrete_proxies(solutionIndex), proxyIndex);
}

template<typename T> void KMeansEvaluator<T>::setMeasure(DissimilarityMeasure<T> const *measure)
{
	if(measure)
		this->measure = 0;
	else
		this->measure = measure->clone();
}


//TODO handle numOfProxies==0
template<typename T> std::vector<double> KMeansEvaluator<T>::proxycostGeneric(std::vector<T*> const& points, std::vector<T> const& proxies) const
{
	int numOfPoints = points.size();
	int numOfProxies = proxies.size();

	std::vector<double> result(numOfProxies, 0);

	for(int i = 0; i < numOfPoints; i++)
	{
		T* point = points[i];

		double min = this->measure->dissimilarity(*point, proxies[0]);
		int assignedProxy = 0;
		for(int j = 1; j < numOfProxies; j++)
		{
			T proxy = proxies[j];
			double candidate = this->measure->dissimilarity(*point, proxy);
			if(candidate < min)
			{
				min = candidate;
				assignedProxy = j;
			}
		}
		if(weightModifier != 0)
			min *= weightModifier->getWeight(*point);
		result[assignedProxy] += min;
	}

	return result;
}

//TODO handle numOfProxies==0
template<typename T> std::vector<double> KMeansEvaluator<T>::proxycostGeneric(std::vector<T*> const& points, std::vector<T*> const& proxies) const
{
	int numOfPoints = points.size();
	int numOfProxies = proxies.size();

	std::vector<double> result(numOfProxies, 0);

	for(int i = 0; i < numOfPoints; i++)
	{
		T* point = points[i];

		double min = this->measure->dissimilarity(*point, *proxies[0]);
		int assignedProxy = 0;
		for(int j = 1; j < numOfProxies; j++)
		{
			T* proxy = proxies[j];
			double candidate = this->measure->dissimilarity(*point, *proxy);
			if(candidate < min)
			{
				min = candidate;
				assignedProxy = j;
			}
		}
		if(weightModifier != 0)
			min *= weightModifier->getWeight(*point);
		result[assignedProxy] += min;
	}

	return result;
}

//### CombinedEvaluation members

template<typename T> double KMeansEvaluator<T>::combinedcost(std::vector<std::vector<T*> > const& clusters, std::vector<T> const& proxies) const
{
	double sum = 0;

	int numOfClusters = clusters.size();
	int numOfProxies = proxies.size();
	int minNumOfClustersProxies = numOfClusters < numOfProxies ? numOfClusters : numOfProxies;

	for(int i = 0; i < minNumOfClustersProxies; i++)
	{
		sum += combinedcost(clusters[i], proxies[i]);
	}

	if(numOfClusters < numOfProxies)
	{
		//std::clog << "CluE::KMeansEvaluator<T>::combinedcost(std::vector<std::vector<T*> >, std::vector<T>) - WARNING: More proxies than clusters: ignoring redundant proxies." << std::endl;
	}
	else if(numOfClusters > numOfProxies)
	{
		//std::clog << "CluE::KMeansEvaluator<T>::combinedcost(std::vector<std::vector<T*> >, std::vector<T>) - WARNING: Less proxies than clusters: assigning remaining points to proxies." << std::endl;
		for(int i = numOfProxies; i < numOfClusters; i++)
		{
			sum += proxycost(clusters[i], proxies);
		}
	}

	return sum;
}

template<typename T> double KMeansEvaluator<T>::combinedcost(std::vector<std::vector<T*> > const& clusters, std::vector<T*> const& proxies) const
{
	double sum = 0;

	int numOfClusters = clusters.size();
	int numOfProxies = proxies.size();
	int minNumOfClustersProxies = numOfClusters < numOfProxies ? numOfClusters : numOfProxies;

	for(int i = 0; i < minNumOfClustersProxies; i++)
	{
		sum += CombinedEvaluation<T>::combinedcost(clusters[i], proxies[i]);
	}

	if(numOfClusters < numOfProxies)
	{
		//std::clog << "CluE::KMeansEvaluator<T>::combinedcost(std::vector<std::vector<T*> >, std::vector<T*>) - WARNING: More proxies than clusters: ignoring redundant proxies." << std::endl;
	}
	else if(numOfClusters > numOfProxies)
	{
		//std::clog << "CluE::KMeansEvaluator<T>::combinedcost(std::vector<std::vector<T*> >, std::vector<T*>) - WARNING: Less proxies than clusters: assigning remaining points to proxies." << std::endl;
		for(int i = numOfProxies; i < numOfClusters; i++)
		{
			sum += proxycost(clusters[i], proxies);
		}
	}

	return sum;
}

template<typename T> double KMeansEvaluator<T>::combinedcost(PartitionProvider<T> const &clusterProvider, ProxyProvider<T> const &proxyProvider, unsigned int solutionIndex) const
{
	return combinedcost(clusterProvider.clustering(solutionIndex), proxyProvider.proxies(solutionIndex));
}

template<typename T> double KMeansEvaluator<T>::combinedcost(PartitionProvider<T> const &clusterProvider, DiscreteProxyProvider<T> const &proxyProvider, unsigned int solutionIndex) const
{
	return combinedcost(clusterProvider.clustering(solutionIndex), proxyProvider.discrete_proxies(solutionIndex));
}

template<typename T> double KMeansEvaluator<T>::combinedcost(std::vector<T*> const& cluster, T const& proxy) const
{
	double sum = 0;

	int numOfPoints = cluster.size();
	for(int i = 0; i < numOfPoints; i++)
	{
		double dist = this->measure->dissimilarity(*cluster[i], proxy);
		if(weightModifier != 0)
			dist *= weightModifier->getWeight(*cluster[i]);
		sum += dist;
	}

	return sum;
}

template<typename T> double KMeansEvaluator<T>::combinedcost(PartitionProvider<T> const &clusterProvider, ProxyProvider<T> const &proxyProvider, unsigned int solutionIndex, unsigned int proxyIndex) const
{
	return combinedcost(clusterProvider.cluster(solutionIndex, proxyIndex), proxyProvider.proxy(solutionIndex, proxyIndex));
}

template<typename T> double KMeansEvaluator<T>::combinedcost(PartitionProvider<T> const &clusterProvider, DiscreteProxyProvider<T> const &proxyProvider, unsigned int solutionIndex, unsigned int proxyIndex) const
{
	return CombinedEvaluation<T>::combinedcost(clusterProvider.cluster(solutionIndex, proxyIndex), proxyProvider.discrete_proxy(solutionIndex, proxyIndex));
}

template<typename T> void KMeansEvaluator<T>::setWeightModifier(WeightModifier<T>* wm)
{
	if(wm != 0)
		weightModifier = wm->clone();
	else
		wm = 0;
}

}

#endif
