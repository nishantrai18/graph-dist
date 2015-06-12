#include <cstdio>
#include <iostream>
#include <algorithm>
#include <queue>
#include <vector>
#include <cstdlib>
#include <ctime>
#include "random_graph.h"

using namespace std;

#define INF 10000000000
#define N 1000

bool operator <( edge a, edge b ) 
{
	return a.weight < b.weight;
}

void dijkstra( vector < vector < edge > > graph, float dist[], int size, int node ); 
void bidijkstra( vector < vector < edge > > graph, vector < vector < edge > > revgraph, int visa[], int visb[], float dista[], float distb[], int size, int na, int nb );

int main() 
{
	srand (static_cast <unsigned> (time(0)));

	int size,m,i,j,x,y,iters;
	
	vector <vector <edge> > graph,revgraph;

	float dista[N+10],distb[N+10];
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
        t.end_node=x;
        t.weight=a;
        graph[y].push_back(t);
    }*/
   
    undir_random_graph(graph,size,threshold);   

    for(i=1;i<=size;i++)
    {
    	for(j=0;j<graph[i].size();j++)
    	{
    		edge t;
    		t.end_node=i;
    		t.weight=graph[i].weight;
    		revgraph[graph[i].end_node].push_back(t);
    	}
    }

    /*for(i=1;i<=size;i++)
    {
    	cout<<i<<": "<<graph[i].size();
    	for(j=0;j<graph[i].size();j++)
    	{
    		cout<<graph[i][j].end_node<<":";
    		printf("%d, ",(int) (graph[i][j].weight));
    	}
    	cout<<"\n";
    }*/

	for ( i = 0; i <= size; ++i )
	{
		dista[i] = INF;
		distb[i] = INF;
		visa[i]=visb[i]=0;
	}

	int S,T;
	scanf( "%d %d", &S, &T );
	
	bidijkstra( graph, revgraph, visa, visb, dista, distb, size, S, T );

	return 0;
}


void dijkstra( vector < vector < edge > > graph, float dist[], int size, int node ) 
{
	dist[node] = 0;
	priority_queue < edge > q;
	q.push( ( edge ) { node, 0 } );

	while ( !q.empty() ) {
		edge p = q.top();
		q.pop();
		for ( int i = 0; i<graph[p.end_node].size(); ++i )
		{
			int u = p.end_node;
			int v = graph[p.end_node][i].end_node;
			
			if ((dist[u]+graph[p.end_node][i].weight)<dist[v])
			{
				dist[ v ] = dist[ u ] + graph[ p.end_node ][ i ].weight;
				q.push( graph[ p.end_node ][ i ] );
			}
		}
	}
}

float bidijkstra( vector < vector < edge > > graph, int visa[], int visb[], float dista[], float distb[], int size, int na, int nb )
{
	float ans=INF;

	dista[na] = 0;
	distb[nb] = 0;
	
	visa[na] = 1;
	visb[nb] = 1;
	
	priority_queue < edge > qa;
	priority_queue < edge > qb;
	
	qa.push( (edge) { na, 0 } );
	qb.push( (edge) { nb, 0 } );

	int common=0;

	while ((!common)&&(!qa.empty())&&(!qb.empty())) 
	{
		edge p = qa.top();
		int u = p.end_node;
		qa.pop();
		for ( int i = 0; i<graph[p.end_node].size(); ++i )
		{
			int v = graph[p.end_node][i].end_node;
			if ((dista[u]+graph[p.end_node][i].weight)<dista[v])
			{
				dista[ v ] = dista[ u ] + graph[ p.end_node ][ i ].weight;
				visa[v]=1;
				qa.push( graph[ p.end_node ][ i ] );
			}
			if(visb[v]==1)
				common=1;
		}

		p = qb.top();
		u = p.end_node;
		qb.pop();
		for ( int i = 0; i<revgraph[p.end_node].size(); ++i )
		{
			int v = revgraph[p.end_node][i].end_node;
			if ((distb[u]+revgraph[p.end_node][i].weight)<distb[v])
			{
				distb[ v ] = distb[ u ] + revgraph[ p.end_node ][ i ].weight;
				visb[v]=1;
				qb.push( revgraph[ p.end_node ][ i ] );
			}
			if(visa[v]==1)
				common=1;
		}

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
	
	}
} 