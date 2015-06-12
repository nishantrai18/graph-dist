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

int floyd_warshall(vector <vector <edge> > gr, float fwdist[][210], int sz);
void dijkstra( vector < vector < edge > > graph, float dist[], int size, int node ); 
float bidijkstra( vector < vector < edge > > graph, vector < vector < edge > > revgraph, int visa[], int visb[], float dista[], float distb[], 
	int size, int na, int nb );

int main() 
{
	srand (static_cast <unsigned> (time(0)));

	int size,m,i,j,x,y,iters;
	
	vector <vector <edge> > graph,revgraph;

	float dista[N+10],distb[N+10],fwdist[210][210],ans;
	int visa[N+10],visb[N+10];

    float threshold=0.2; 
	
	cout<<"Input Size, probability: ";
    cin>>size>>m;

    int sum=0,tmp=iters;

    graph.resize(size+2);
    revgraph.resize(size+2);

    for(i=0;i<m;i++)
    {
    	int x,y;
    	float a;
        scanf("%d %d %f",&x,&y,&a);
        edge t;
        t.end_node=y;
        t.weight=a;
        graph[x].push_back(t);
    }
   
    //undir_random_graph(graph,size,threshold);   

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
    }*/

    cout<<"\n";

	int S,T;
	//scanf( "%d %d", &S, &T );
	
	floyd_warshall(graph,fwdist,size);

	for ( i = 0; i <= size; ++i )
	{
		dista[i] = INF;
		distb[i] = INF;
		visa[i]=visb[i]=0;
	}

	for(T=1;T<=size;T++)
	for(S=1;S<=size;S++)
	{
		for ( i = 0; i <= size; ++i )
		{
			dista[i] = INF;
			distb[i] = INF;
			visa[i]=visb[i]=0;
		}
		ans=bidijkstra( graph, revgraph, visa, visb, dista, distb, size, S, T );
		if((fwdist[S][T]-ans)*(fwdist[S][T]-ans)>0.0001)
		{
			cout<<T<<" and "<<S<<" with "<<fwdist[S][T]<<":"<<ans<<"\n";
		}		
		//else cout<<T<<" and "<<S<<" with "<<fwdist[T][S]<<":"<<ans<<" are good boys\n";
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
	cout<<" REAK\n";
	dist[node] = 0;
	priority_queue < edge > q;
	q.push( ( edge ) { node, 0 } );

	while ( !q.empty() ) {
		edge p = q.top();
		q.pop();
		cout<<p.end_node<<" : "<<p.weight<<" : "<<dist[p.end_node]<<" AHEM\n";

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

float bidijkstra( vector < vector < edge > > graph, vector < vector < edge > > revgraph, int visa[], int visb[], float dista[], float distb[], int size, int na, int nb )
{
	float ans=INF,a,b,ra=0,rb=0;

	dista[na] = 0;
	distb[nb] = 0;
	
	if(na==nb)
		return 0;
	
	priority_queue < edge > qa, qb;
	
	qa.push( (edge) { na, 0 } );
	qb.push( (edge) { nb, 0 } );

	int common=0;

	while (((ra+rb)<ans)&&(!qa.empty())&&(!qb.empty())) 
	{
		edge p = qa.top();

		//cout<<p.end_node<<" : "<<p.weight<<"\n";

		int u = p.end_node;
		
		if(visa[u])
		{
			qa.pop();
			continue;
		}

		ra=dista[u];
		visa[u] = 1;
		
		qa.pop();
		for ( int i = 0; i<graph[u].size(); ++i )
		{
			int v = graph[u][i].end_node;
			if((dista[u]+graph[u][i].weight)<dista[v])
			{
				dista[ v ] = dista[ u ] + graph[u][ i ].weight;
				a=0;
				if(visb[v])
					{
					a=dista[u]+distb[v];
				}	
				if(a<ans)
					qa.push((edge) {v,dista[v]});
			}
			if(visb[v]==1)
			{
				a=dista[u]+distb[v]+graph[u][i].weight;
				if(a<ans)
					ans=a;	
			}	
		}

		p = qb.top();
		u = p.end_node;
		
		//cout<<p.end_node<<" : "<<p.weight<<"\n";
		if(visb[u])
		{
			qb.pop();
			continue;
		}

		rb=distb[u];		
		visb[u] = 1;

		qb.pop();
		for ( int i = 0; i<revgraph[u].size(); ++i )
		{
			int v = revgraph[u][i].end_node;
			if ((distb[u]+revgraph[u][i].weight)<distb[v])
			{
				distb[ v ] = distb[ u ] + revgraph[u][ i ].weight;
				a=0;
				if(visa[v])
					{
					a=dista[u]+distb[v];
				}	
				if(a<ans)
					qb.push((edge) {v,distb[v]});
			}
			if(visa[v]==1)
			{
				a=distb[u]+dista[v]+revgraph[u][i].weight;
				if(a<ans)
					ans=a;	
			}
		}

		/*cout<<ra+rb<<":"<<ans<<"\n"	;
		
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

	return ans;
} 