#include <cstdio>
#include <iostream>
#include <algorithm>
#include <queue>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <limits>
#include "random_graph.h"

using namespace std;

#define INF 1000000
#define N 1000

bool operator <( edge a, edge b ) 
{
	return a.weight > b.weight;
}

float landmarks_for[20][N+10],landmarks_rev[20][N+10],forcost[N+10],revcost[N+10];

int floyd_warshall(vector <vector <edge> > gr, float fwdist[][210], int sz);
void dijkstra( vector < vector < edge > > graph, float dist[], int size, int node ); 
float biastar( vector < vector < edge > > graph, vector < vector < edge > > revgraph, float forcost[], float revcost[], int visa[], int visb[], float dista[], float distb[], 
	int size, int na, int nb );

int main() 
{
	srand (static_cast <unsigned> (time(0)));

	int size,m,i,j,x,y,iters;
	
	vector <vector <edge> > graph,revgraph;

	float dista[N+10],distb[N+10],forpi[N+10],revpi[N+10],fwdist[210][210],ans;
	int visa[N+10],visb[N+10];

    float threshold=0.2; 
	
	cout<<"Input Size, probability: ";
    cin>>size>>threshold;

    int sum=0,tmp=iters;

    graph.resize(size+2);
    revgraph.resize(size+2);

    /*for(i=0;i<m;i++)
    {
    	int x,y;
    	float a;
        scanf("%d %d %f",&x,&y,&a);
        edge t;
        t.end_node=y;
        t.weight=a;
        graph[x].push_back(t);
    }*/
   
    dir_random_graph(graph,size,threshold);

    for(i=0;i<=size;i++)
    {
    	for(j=0;j<graph[i].size();j++)
    	{
    		edge t;
    		t.end_node=i;
    		t.weight=graph[i][j].weight;
    		revgraph[graph[i][j].end_node].push_back(t);
    	}
    }

    /*for(i=1;i<=size;i++)
    {
    	for(j=0;j<graph[i].size();j++)
    	{
    		printf("%d %d %d\n",i,graph[i][j].end_node, (int) (graph[i][j].weight));
    	}
    }

    cout<<"\n";
	*/
	int S,T;
	//scanf( "%d %d", &S, &T );
	
	cout<<"HOLO\n";

	floyd_warshall(graph,fwdist,size);


	cout<<"HOLO\n";

	int num_landmarks=5;
	int jump=(size/num_landmarks);
	for(i=1;i<=num_landmarks;i++)
	{
		j=(i*jump);
		for(x=0;x<=size;x++)
		{
			landmarks_for[i][x]=landmarks_rev[i][x]=INF;
		}
		dijkstra(graph,landmarks_for[i],size,j);
		dijkstra(revgraph,landmarks_rev[i],size,j);
	}

	for ( i = 0; i <= size; ++i )
	{
		dista[i] = INF;
		distb[i] = INF;
		visa[i]=visb[i]=0;
		forpi[i]=revpi[i]=0;
	}

	cout<<"HOLO\n";

	for(T=1;T<=size;T++)
	for(S=1;S<=size;S++)
	{
		int na=S,nb=T;
		
		for ( i = 0; i <= size; ++i )
		{
			dista[i] = INF;
			distb[i] = INF;
			visa[i]=visb[i]=0;
			forpi[i]=revpi[i]=0;
		}

		for(i=1;i<=(num_landmarks/2);i++)
		{
			j=(i*jump);
			for(x=0;x<=size;x++)
			{
				forpi[x]=max(forpi[x],landmarks_for[i][nb]-landmarks_for[i][x]);
				forpi[x]=max(forpi[x],landmarks_rev[i][x]-landmarks_rev[i][nb]);
				revpi[x]=max(revpi[x],landmarks_for[i][x]-landmarks_for[i][na]);
				revpi[x]=max(revpi[x],landmarks_rev[i][na]-landmarks_rev[i][x]);
			}
		}

		for(x=0;x<=size;x++)
		{
			forcost[x]=((forpi[x]-revpi[x]+revpi[nb])/2);
			revcost[x]=((revpi[x]-forpi[x]+forpi[na])/2);
			//cout<<x<<" : "<<forcost[x]<<" , "<<revcost[x]<<"\n";
		}		

		ans=biastar( graph, revgraph, forcost, revcost, visa, visb, dista, distb, size, S, T );
		
		if(((fwdist[S][T]-ans)*(fwdist[S][T]-ans)>0.0001))
		{
			cout<<T<<" and "<<S<<" with "<<fwdist[S][T]<<":"<<ans<<"\n";
			/*for(x=0;x<=size;x++)
			{
				cout<<x<<" : "<<forcost[x]<<" , "<<revcost[x]<<"\n";
			}*/
		}		
		//else cout<<":";
		//cout<<T<<" and "<<S<<" with "<<fwdist[T][S]<<":"<<ans<<" are good boys\n";
	}
	
	/*S=1,T=5;
	cout<< "THE DISTANCE IS : "<<bidijkstra( graph, revgraph, visa, visb, dista, distb, size, S, T )<<"\n";
	*/
	return 0;
}


int floyd_warshall(vector <vector <edge> > gr, float dist[][210], int sz)
{
	int i,j,k,t;
	float a,b;
	
	for(i=1;i<=sz;i++)
	{
		for(j=1;j<=sz;j++)
		{
			dist[i][j]=INF;
		}
	}

	for(i=1;i<=sz;i++)
	{
		dist[i][i]=0;
		for(j=0;j<gr[i].size();j++)
		{
			dist[i][gr[i][j].end_node]=gr[i][j].weight;
		}
	}

	for(k=1;k<=sz;k++)
	{
		for(i=1;i<=sz;i++)
		{
			for(j=1;j<=sz;j++)
			{
				if(dist[i][j]>(dist[i][k]+dist[k][j]))
				{
					dist[i][j]=dist[i][k]+dist[k][j];
				}
			}
		}
	}

	return 0;
}

void dijkstra( vector < vector < edge > > graph, float dist[], int size, int node ) 
{
	//cout<<" REAK\n";
	dist[node] = 0;
	priority_queue < edge > q;
	q.push( ( edge ) { node, 0 } );

	while ( !q.empty() ) {
		edge p = q.top();
		q.pop();
		//cout<<p.end_node<<" : "<<p.weight<<" : "<<dist[p.end_node]<<" AHEM\n";

		for ( int i = 0; i<graph[p.end_node].size(); ++i )
		{
			int u = p.end_node;
			int v = graph[p.end_node][i].end_node;
			
			if ((dist[u]+graph[p.end_node][i].weight)<dist[v])
			{
				dist[ v ] = dist[ u ] + graph[ p.end_node ][ i ].weight;
				q.push( (edge) {v,dist[v]} );
				//q.push( graph[ p.end_node ][ i ] );
			}
		}
	}
}

float biastar( vector < vector < edge > > graph, vector < vector < edge > > revgraph, float forcost[], float revcost[], int visa[], int visb[], float dista[],
 float distb[], int size, int na, int nb )
{
	//cout<<"BREAK\n";
	float ans=INF,a,b,ra=0,rb=0,corr;

	dista[na] = 0;
	distb[nb] = 0;
	
	if(na==nb)
		return 0;
	
	//cout<<na<<" and "<<nb<<"\n";

	priority_queue < edge > qa;
	priority_queue < edge > qb;
	
	qa.push( (edge) { na, dista[na]} );
	qb.push( (edge) { nb, distb[nb]} );

	int common=0;

	while (((ra+rb)<(ans+forcost[na]-forcost[nb]))&&(!qa.empty())&&(!qb.empty())&&!common) 
	{
		edge p = qa.top();
		int u = p.end_node;
		//cout<<p.end_node<<" : "<<p.weight<<" A\n";

		if(visa[u])
		{
			qa.pop();
			continue;
		}

		ra=p.weight;

		visa[u] = 1;
		if(visb[u])
			{
			common=1;
			a=dista[u]+distb[u];
			if(a<ans)
				ans=a;	
		}	
		
		qa.pop();
		for ( int i = 0; i<graph[u].size(); ++i )
		{
			int v = graph[u][i].end_node;
			corr=forcost[v]-forcost[u];
			if((dista[u]+graph[u][i].weight+corr)<dista[v])
			{
				dista[ v ] = dista[ u ] + graph[u][ i ].weight + corr ;
				if(visb[v]==0)
					qa.push((edge) {v,dista[v]});
			}
			if(visb[v]==1)
			{
				a=dista[u]+distb[v]+graph[u][i].weight + corr ;
				if(a<ans)
					ans=a;	
			}	
		}

		p = qb.top();
		u = p.end_node;
		//cout<<p.end_node<<" : "<<p.weight<<" B\n";

		if(visb[u])
		{
			qb.pop();
			continue;
		}
	
		rb=p.weight;				

		visb[u] = 1;
		if(visa[u])
			{
			common=1;
			a=dista[u]+distb[u];
			if(a<ans)
				ans=a;	
		}	

		qb.pop();
		for ( int i = 0; i<revgraph[u].size(); ++i )
		{
			int v = revgraph[u][i].end_node;
			corr=revcost[v]-revcost[u];
			if ((distb[u]+revgraph[u][i].weight+corr)<distb[v])
			{
				distb[ v ] = distb[ u ] + revgraph[u][ i ].weight + corr;
				if(visa[v]==0)
					qb.push((edge) {v,distb[v]});
			}
			if(visa[v]==1)
			{
				a=distb[u]+dista[v]+revgraph[u][i].weight+corr;
				if(a<ans)
					ans=a;	
			}
		}

		/*cout<<ra+rb<<":"<<ans+revcost[nb]<<"\n"	;
		
		for(int i=1;i<=size;i++)
		{
			printf("%d:%d, ",i,(int)dista[i]);
		}
		printf("\tThis was dist a\n");
		for(int i=1;i<=size;i++)
		{
			printf("%d:%d, ",i,(int)distb[i]);
		}
		printf("\tThis was dist b\n");


		for(int i=1;i<=size;i++)
		{
			if(visa[i])
			printf("%d, ",i);
		}
		printf("\tThis was visa\n");
		for(int i=1;i<=size;i++)
		{
			if(visb[i])
			printf("%d, ",i);
		}
		printf("\tThis was visb\n");
		*/
	}

	return ans+forcost[na]-forcost[nb];
} 