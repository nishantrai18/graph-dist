#include <cstdio>
#include <iostream>
#include <algorithm>
#include <cmath>
#include <queue>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <limits>
#include <map>
#include "random_graph.h"

using namespace std;

#define INF 100000000
#define N 1100000

bool operator <( edge a, edge b ) 
{
	return a.weight > b.weight;
}

map<long long int, int> vertices;

int iters=0;

int stat[N+10];
float landmarks_for[120][N+10],landmarks_rev[120][N+10],forcost[N+10],revcost[N+10];
float dista[N+10],distb[N+10],forpi[N+10],revpi[N+10],ans;
int visa[N+10],visb[N+10];

void dijkstra( vector < vector < edge > > graph, float dist[], int size, int node ); 
float biastar( vector < vector < edge > > graph, vector < vector < edge > > revgraph, float forcost[], float revcost[],
			   int visa[], int visb[], float dista[], float distb[], int size, int na, int nb );
float bidijkstra( vector < vector < edge > > graph, vector < vector < edge > > revgraph, int visa[], int visb[], 
				  float dista[], float distb[], int size, int na, int nb );

int main() 
{
	srand (static_cast <unsigned> (time(0)));

	int size,m,i,j,x,y,v=1;
	
	vector <vector <edge> > graph,revgraph;

    float threshold=0.2; 
	
	char c;
	cin>>c;
	cout<<"Input Size, Edges: ";
    cin>>size>>m;

    int sum=0,tmp;

    graph.resize(size+2);
    revgraph.resize(size+2);

	for(i=0;i<m;i++)
    {
    	cin>>c;
		int x,y,mx,my;
    	float a;
        scanf("%d %d %f",&x,&y,&a);
        if(vertices.find(x)==vertices.end())
        {
        	vertices.insert(make_pair(x,v));
        	v++;
        }
        if(vertices.find(y)==vertices.end())
        {
        	vertices.insert(make_pair(y,v));
        	v++;
        }
        edge t;
        t.end_node=vertices[y];
        t.weight=a;
        graph[vertices[x]].push_back(t);
        //epair p1 (x,y);
        //lengths.insert(make_pair(p1,a));
        /*t.end_node=x;
        t.weight=a;
        graph[y].push_back(t);
        epair p2 (y,x);
        lengths.insert(make_pair(p2,a));
    	*/
    }

    /*for(i=0;i<m;i++)
    {
    	int x,y;
    	float a;
    	cin>>c;
        scanf("%d %d %f",&x,&y,&a);
        edge t;
        t.end_node=y;
        t.weight=a;
        graph[x].push_back(t);
    }*/
	
    //dir_random_graph(graph,size,threshold);

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
	
	/*for(i=1;i<=size;i++)
    {
    	for(j=0;j<graph[i].size();j++)
    	{
    		printf("%d ",graph[i][j].end_node);
    	}
    	cout<<"\n";
	}
	*/
    cout<<"Number of runs: ";
	


	int S,T;

	int num_landmarks=64;
	int jump=(size/num_landmarks);
	
	for(i=1;i<=num_landmarks+1;i++)
	{
		for(x=0;x<=size;x++)
		{
			landmarks_for[i][x]=0;
		}	
	}

	for(x=0;x<=size;x++)
	{
		stat[x]=1;
	}


	cout<<"REACHED THE PREPROCESSING FOR ALT\n";

	for(i=1;i<=num_landmarks;i++)
	{
		float mx=0;
		j=1;
		for(x=1;x<=size;x++)
		{
			if(stat[x]&&(landmarks_for[num_landmarks+1][x]>mx))
			{
				j=x;
				mx=landmarks_for[num_landmarks+1][x];
			}
		}	
		stat[j]=0;

		cout<<"J IS "<<j<<"\n";

		for(x=0;x<=size;x++)
		{
			landmarks_for[i][x]=landmarks_rev[i][x]=INF;
		}

		dijkstra(graph,landmarks_for[i],size,j);
		dijkstra(revgraph,landmarks_rev[i],size,j);

		for(x=0;x<=size;x++)
		{
			landmarks_for[num_landmarks+1][x]+=sqrt(landmarks_for[i][x]);
		}
		cout<<i<<" is done\n";
	}
	

	/*
	for(i=1;i<=num_landmarks;i++)
	{
		j=(i*jump);
		for(x=0;x<=size;x++)
		{
			landmarks_for[i][x]=landmarks_rev[i][x]=INF;
		}
		dijkstra(graph,landmarks_for[i],size,j);
		dijkstra(revgraph,landmarks_rev[i],size,j);

		cout<<i<<" is done\n";
	}
	*/
	for ( i = 0; i <= size; ++i )
	{
		dista[i] = INF;
		distb[i] = INF;
		visa[i]=visb[i]=0;
		forpi[i]=revpi[i]=0;
	}

	cout<<"Number of runs: ";
	int runs;
	//cin>>runs;
	runs=1000;
	tmp=runs;
	int arun=0,drun=0;

	while(runs>0)
	{
		S=rand()%size+1;
		T=rand()%size+1;
		
		//cout<<S<<" and "<<T<<"\n";
		if(S==T)
			continue;

		int na=S,nb=T;

		for ( i = 0; i <= size; ++i )
		{
			dista[i] = INF;
			distb[i] = INF;
			visa[i]=visb[i]=0;
			forpi[i]=revpi[i]=0;
		}

		for(i=0;i<20;i++)
			stat[i]=0;
		
		for(i=1;i*i<=(num_landmarks);i++)
		{
			float mx=0,a,b,cst=0;
			j=1;
			for(x=1;x<=num_landmarks;x++)
			{
				a=max(landmarks_rev[x][na]-landmarks_rev[x][nb],landmarks_for[x][nb]-landmarks_for[x][na]);
				b=max(landmarks_for[x][nb]-landmarks_for[x][na],landmarks_rev[x][na]-landmarks_rev[x][nb]);
				cst=a+b;	
				if((cst>mx)&&(stat[x]==0))
				{
					j=i;
					mx=cst;
				}
			}	
			stat[j]=1;
			for(x=0;x<=size;x++)
			{
				forpi[x]=max(forpi[x],landmarks_for[j][nb]-landmarks_for[j][x]);
				forpi[x]=max(forpi[x],landmarks_rev[j][x]-landmarks_rev[j][nb]);
				revpi[x]=max(revpi[x],landmarks_for[j][x]-landmarks_for[j][na]);
				revpi[x]=max(revpi[x],landmarks_rev[j][na]-landmarks_rev[j][x]);
			}
		}

		for(x=0;x<=size;x++)
		{
			forcost[x]=((forpi[x]-revpi[x]+revpi[nb])/2);
			revcost[x]=((revpi[x]-forpi[x]+forpi[na])/2);
			//cout<<x<<" : "<<forcost[x]<<" , "<<revcost[x]<<"\n";
		}		

		iters=0;
		ans=biastar( graph, revgraph, forcost, revcost, visa, visb, dista, distb, size, S, T );
		arun+=iters;

		for ( i = 0; i <= size; ++i )
		{
			dista[i] = INF;
			distb[i] = INF;
			visa[i]=visb[i]=0;
		}
		
		iters=0;
		ans=bidijkstra( graph, revgraph, visa, visb, dista, distb, size, S, T );
		drun+=iters;

		runs--;

		if(runs%20==0)
		{
			cout<<"RUNS ARE : "<<runs<<"\n";
			cout<<"THE AVERAGE ITERATION FOR A* IS : "<<((arun*(1.0))/(tmp-runs))<<"\n";
			cout<<"THE AVERAGE ITERATION FOR BDV IS : "<<((drun*(1.0))/(tmp-runs))<<"\n";
		}
		//else cout<<":";
		//cout<<T<<" and "<<S<<" with "<<fwdist[T][S]<<":"<<ans<<" are good boys\n";
	}
	
	cout<<"THE AVERAGE ITERATION FOR A* IS : "<<((arun*(1.0))/tmp)<<"\n";

	cout<<"THE AVERAGE ITERATION FOR BDV IS : "<<((drun*(1.0))/tmp)<<"\n";

	/*S=1,T=5;
	cout<< "THE DISTANCE IS : "<<bidijkstra( graph, revgraph, visa, visb, dista, distb, size, S, T )<<"\n";
	*/
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

	while (((ra+rb)<(ans+revcost[nb]+forcost[nb]))&&(!qa.empty())&&(!qb.empty())&&!common) 
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

		iters++;

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

	//cout<<"THE NUMBER IS : "<<iters<<"\n";
	return ans+forcost[na]-forcost[nb];
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

		iters++;

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

	//cout<<"THE OTHER NUMBER IS : "<<iters<<"\n";
	return ans;
} 