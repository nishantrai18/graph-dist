#include <iostream>
#include <algorithm>
#include <cstdio>
#include <queue>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <limits>
#include <map>
#include "random_graph.h"

using namespace std;

#define INF 1000000
#define N 1000

typedef pair< int , int> epair;

bool operator <( edge a, edge b ) 
{
	return a.weight > b.weight;
}

////////////////

map<epair, float> lengths;

vector <vector <edge> > graph,revgraph,prevtree,foratree;
vector <int> prev,fora;

float dista[N+10],distb[N+10],opt;
int visa[N+10],visb[N+10],sz,size;

//////////////////

int graph_print(vector <vector <edge> > graph, int size)
{
	int i,j,t;
	for(i=0;i<=size;i++)
    {
    	for(j=0;j<graph[i].size();j++)
    	{
    		cout<<graph[i][j].end_node<<":";
    	}
    	cout<<"\n";
    }
    return 0;
}

float bidijkstra_forlength(float dista[], float distb[], int visa[], int visb[], int na, int nb );
float bidijkstra(float limit, int na, int nb );
int alt_path(int na, int nb, float share, float stretch, float localt);
int check_validity(vector <int> pathab, vector <int> pathacb, int index_nc, int na, int nc, int nb, float share_amt_prev[], float share_amt_fora[], 
				   float share, float stretch, float localt);
int findpath(vector <int> &path, int na, int nc, int nb);
float chshare(vector <int> patha, vector <int> pathb);
int chlocopt(vector <int> pathacb, int index_nc, int na, int nc, int nb, float param_t);
int chstretch(vector <int> pathacb, int index_nc, int na, int nc, int nb, float stretch);
int findshare(vector <int> optpath, float share_amt_prev[], float share_amt_fora[], int na, int nb);
void dfs(int fg, int n, vector <int> optpath, vector <vector <edge> > arr, float share_amt[]);

int main()
{
	srand (static_cast <unsigned> (time(0)));

	int m,i,j,x,y,iters;

    sz=6;
    cout<<"Input Size, edges : ";
    cin>>sz;
    
    size=sz;
    float share,stretch,localt,threshold;
	
	share=0.2,stretch=0.5,localt=0.1;
	threshold=0.05;
    /*cout<<"Enter parameters : share, stretch, localt: ";
    cin>>share>>stretch>>localt;

    cout<<"Enter threshold for random graph: ";
    cin>>threshold;
	*/

    int sum=0,tmp=iters;

    graph.resize(sz+2);
    revgraph.resize(sz+2);
    prev.resize(sz+2);
    fora.resize(sz+2);

    /*for(i=0;i<m;i++)
    {
    	int x,y;
    	float a;
        scanf("%d %d %f",&x,&y,&a);
        edge t;
        t.end_node=y;
        t.weight=a;
        graph[x].push_back(t);
        epair p1 (x,y);
        lengths.insert(make_pair(p1,a));
        t.end_node=x;
        t.weight=a;
        graph[y].push_back(t);
        epair p2 (y,x);
        lengths.insert(make_pair(p2,a));
    }*/

    undir_random_graph(graph,size,threshold);   

    for(i=1;i<=size;i++)
    {
    	for(j=0;j<graph[i].size();j++)
    	{
    		int x=i,y=graph[i][j].end_node;
    		float a=graph[i][j].weight;
    		epair p1 (x,y);
	        lengths.insert(make_pair(p1,a));
	        epair p2 (y,x);
	        lengths.insert(make_pair(p2,a));
    	}
    }


	/*for(i=1;i<=size;i++)
    {
    	for(j=0;j<graph[i].size();j++)
    	{
    		if(graph[i][j].end_node>=i)
    			break;
    		printf("%d %d %d\n",i,graph[i][j].end_node, (int) (graph[i][j].weight));
    	}
    }
	*/
	for(i=1;i<=size;i++)
    {
    	for(j=0;j<graph[i].size();j++)
    	{
    		if(graph[i][j].end_node>=i)
    			break;
    		printf("%d ",graph[i][j].end_node);
    	}
    	cout<<"\n";
    }

    for(i=0;i<=size;i++)
    {
        epair p1 (0,i);
        lengths.insert(make_pair(p1,0));
        epair p2 (i,0);
        lengths.insert(make_pair(p2,0));
        
    	for(j=0;j<graph[i].size();j++)
    	{
    		edge t;
    		t.end_node=i;
    		t.weight=graph[i][j].weight;
    		revgraph[graph[i][j].end_node].push_back(t);
    	}
    }
	
    cout<<"COOL\n";

	alt_path(1,2,share,stretch,localt);

    return 0;
}

float bidijkstra_forlength(float dista[], float distb[], int visa[], int visb[], int na, int nb )
{
	float ans=INF,a,b,ra=0,rb=0,ma=0,mb=0;

	dista[na] = distb[nb] = 0;
	
	if(na==nb)
		return 0;
	
	priority_queue < edge > qa, qb;
	
	qa.push( (edge) { na, 0 } );
	qb.push( (edge) { nb, 0 } );

	while ((!qa.empty())&&(!qb.empty())&&((ra+rb)<ans)) 
	{
		edge p = qa.top();
		int u = p.end_node;
		//cout<<p.end_node<<" : "<<p.weight<<" A\n";

		ra=dista[u];

		visa[u] = 1;
		if(visb[u])
			{
			a=dista[u]+distb[u];
			if(a<ans)
				ans=a;	
		}	
		
		qa.pop();
		for ( int i = 0; i<graph[u].size(); ++i )
		{
			int v = graph[u][i].end_node;
			if((dista[u]+graph[u][i].weight)<dista[v])
			{
				dista[ v ] = dista[ u ] + graph[u][ i ].weight;
				qa.push((edge) {v,dista[v]});
			}
			if(visb[v]==1)
			{
				a=dista[u]+distb[v]+graph[u][i].weight;
				if(a<ans)
					ans=a;	
			}
			ma=max(ma,dista[v]);	
		}

		p = qb.top();
		u = p.end_node;
		//cout<<p.end_node<<" : "<<p.weight<<" B\n";

		rb=distb[u];		
		visb[u] = 1;
		if(visa[u])
			{
			a=dista[u]+distb[u];
			if(a<ans)
				ans=a;	
		}	

		qb.pop();
		for ( int i = 0; i<revgraph[u].size(); ++i )
		{
			int v = revgraph[u][i].end_node;
			if ((distb[u]+revgraph[u][i].weight)<distb[v])
			{
				distb[ v ] = distb[ u ] + revgraph[u][ i ].weight;
				qb.push((edge) {v,distb[v]});
			}
			if(visa[v]==1)
			{
				a=distb[u]+dista[v]+revgraph[u][i].weight;
				if(a<ans)
					ans=a;	
			}
		}
	}
	return ans;
} 

float bidijkstra( float limit, int na, int nb )
{
	//cout<<"BREAK\n";
	float ans=INF,a,b,ra=0,rb=0,ma=0,mb=0;

	prevtree.clear();
	foratree.clear();
	prevtree.resize(sz+2);
	foratree.resize(sz+2);

	prev[na] = fora[nb] = dista[na] = distb[nb] = 0;
	
	if(na==nb)
		return 0;
	
	priority_queue < edge > qa, qb;
	
	qa.push( (edge) { na, 0 } );
	qb.push( (edge) { nb, 0 } );

	while ((!qa.empty())&&(!qb.empty())&&((ma<(limit*ans))||(mb<(limit*ans))||((ra+rb)<ans))) 
	{
		edge p = qa.top();
		int u = p.end_node;
		//cout<<p.end_node<<" : "<<p.weight<<" A\n";

		//if(visa[u])
		//	continue;

		ra=dista[u];

		visa[u] = 1;
		if(visb[u])
			{
			a=dista[u]+distb[u];
			if(a<ans)
				ans=a;	
		}	
		
		qa.pop();
		for ( int i = 0; i<graph[u].size(); ++i )
		{
			int v = graph[u][i].end_node;
			if((dista[u]+graph[u][i].weight)<dista[v])
			{
				dista[ v ] = dista[ u ] + graph[u][ i ].weight;
				prev[v]=u;
				prevtree[u].push_back(graph[u][i]);
				qa.push((edge) {v,dista[v]});
			}
			if(visb[v]==1)
			{
				a=dista[u]+distb[v]+graph[u][i].weight;
				if(a<ans)
					ans=a;	
			}
			ma=max(ma,dista[v]);	
		}

		p = qb.top();
		u = p.end_node;
		//cout<<p.end_node<<" : "<<p.weight<<" B\n";
		
		//if(visb[u])
		//	continue;


		rb=distb[u];		
		visb[u] = 1;
		if(visa[u])
			{
			a=dista[u]+distb[u];
			if(a<ans)
				ans=a;	
		}	

		qb.pop();
		for ( int i = 0; i<revgraph[u].size(); ++i )
		{
			int v = revgraph[u][i].end_node;
			if ((distb[u]+revgraph[u][i].weight)<distb[v])
			{
				distb[ v ] = distb[ u ] + revgraph[u][ i ].weight;
				fora[v]=u;
				foratree[u].push_back(graph[u][i]);
				qb.push((edge) {v,distb[v]});
			}
			if(visa[v]==1)
			{
				a=distb[u]+dista[v]+revgraph[u][i].weight;
				if(a<ans)
					ans=a;	
			}
			mb=max(mb,distb[v]);
		}

		//cout<<ra+rb<<":"<<ans<<"\n"	;
		/*
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

int alt_path(int na, int nb, float share, float stretch, float localt)
{
	int i,j,t,opv=0;
	
	float mina=INF;

	vector <int> candidates, optpath;
	float share_amt_prev[N+10],share_amt_fora[N+10];

    cout<<"COOL\n";

	for ( i = 0; i <= size; ++i )
	{
		dista[i] = INF;
		distb[i] = INF;
		share_amt_prev[i]=share_amt_fora[i]=0;
		fora[i]=prev[i]=visa[i]=visb[i]=0;
	}

    cout<<"COOL1\n";

	opt=bidijkstra((1+stretch), na, nb);

    cout<<"COOL2\n";

	for ( i = 0; i <= size; ++i )
	{
		if(visa[i]*visb[i]==1)
		{
			candidates.push_back(i);
			if((dista[i]+distb[i])<mina)
			{
				mina=dista[i]+distb[i];
				opv=i;
			}
		}
	}

	/*cout<<"The shortest between "<<na<<" and "<<nb<<" is "<< opt<<"\n";
	cout<<"It goes through "<<opv<<"\n";

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


    cout<<"COOL3\n";
	*/

	findpath(optpath,na,opv,nb);
	
	cout<<"COOL4\n";

	cout<<"The shortest path is : "<<"\n";
	for(int y=0;y<optpath.size();y++)
	{
		cout<<optpath[y]<<" ";
	}	
	cout<<"\n";

	findshare(optpath,share_amt_prev,share_amt_fora,na,nb);

	/*	for(int i=1;i<=size;i++)
		{
			printf("%d:%d, ",i,(int)share_amt_prev[i]);
		}
		printf("\tThis was share a\n");
		for(int i=1;i<=size;i++)
		{
			printf("%d:%d, ",i,(int)share_amt_fora[i]);
		}
		printf("\tThis was share b\n");

	cout<<"COOL5\n";
	*/

	for(i=0;i<candidates.size();i++)
	{
		j=candidates[i];
		if((dista[j]+distb[j])<=((1+stretch)*opt))
		{
			vector <int> pathacb;
			int index_nc=findpath(pathacb,na,j,nb);
			/*cout<<"A candidate path is : "<<na<<" "<<j<<" "<<nb<<"\n";
			for(int y=0;y<pathacb.size();y++)
			{
				cout<<pathacb[y]<<" ";
			}	
			cout<<"\n";
			*/
			t=check_validity(optpath,pathacb,index_nc,na,j,nb,share_amt_prev,share_amt_fora,share,stretch,localt);
			if(t==1)
			{
				cout<<"An alternative path is : "<<na<<" "<<j<<" "<<nb<<"\n";
				for(j=0;j<pathacb.size();j++)
				{
					cout<<pathacb[j]<<" ";
				}	
				cout<<"\n";
			}
		}
	}
	return 0;
}

int findpath(vector <int> &path, int na, int nc, int nb)
{
	int i,j=nc,t,index_nc=0;
	while(j!=0)
	{
		path.push_back(j);
		j=prev[j];
		index_nc++;
	}
	reverse(path.begin(), path.end());
	j=fora[nc];
	while(j!=0)
	{
		path.push_back(j);
		j=fora[j];
	}
	return index_nc-1;
}

int check_validity(vector <int> pathab, vector <int> pathacb, int index_nc, int na, int nc, int nb, float share_amt_prev[], float share_amt_fora[], 
				   float share, float stretch, float localt)
{
	int i,j,t;
	float shr,str,loc;
	
	shr=share_amt_fora[nc]+share_amt_prev[nc];
	
	//cout<<"SHARE : "<<shr<<"\n";

	str=chstretch(pathacb,index_nc,na,nc,nb,stretch);

	//cout<<"STRETCH: "<<str<<"\n";

	loc=chlocopt(pathacb,index_nc,na,nc,nb,localt);

	//cout<<"LOCAL: "<<loc<<"\n";

	if((shr/opt)>share)
		shr=0;
	else
		shr=1;

	return shr*str*loc;

}

int findshare(vector <int> optpath, float share_amt_prev[], float share_amt_fora[], int na, int nb)
{
	int i,j,t;
	vector <int> sorted_path;
	sorted_path=optpath;
	sort(sorted_path.begin(), sorted_path.end());

	share_amt_prev[0]=0;
	share_amt_fora[0]=0;
	
	dfs(1,na,sorted_path,prevtree,share_amt_prev);
	dfs(0,nb,sorted_path,foratree,share_amt_fora);

	return 0;
}

void dfs(int fg, int n, vector <int> optpath, vector <vector <edge> > arr, float share_amt[])
{
	int j,t=1;
	if(fg)
		share_amt[n]=share_amt[prev[n]];
	else
		share_amt[n]=share_amt[fora[n]];
	if(binary_search(optpath.begin(), optpath.end(), n))
	{
		epair p (0,n);
		if(fg)
			p.first=prev[n];
		else
			p.first=fora[n];
		share_amt[n]+=lengths[p];
	}
	for(j=0;j<arr[n].size();j++)
	{
	    dfs(fg,arr[n][j].end_node,optpath,arr,share_amt);
	}
}

float chshare_node(vector <int> patha, vector <int> pathb)
{
	int i=0,j=0,t=0,la=patha.size(),lb=pathb.size();
	sort(patha.begin(), patha.end());
	sort(pathb.begin(), pathb.end());
	
	float shr=0;
	
	while((i<la)&&(j<lb))
	{
		if(patha[i]>patha[j])
			j++;
		else if(patha[i]<patha[j])
			i++;
		else
		{	
			shr=shr+1;	
			i++;
		}	
	}

	return shr;
}

int chlocopt(vector <int> path, int index_nc, int na, int nc, int nb, float param_t)
{
	int cla = na, clb = nb;
	float da = dista[nc], db = distb[nc];

	//cout<<"The parameters is "<<param_t<<" "<<opt<<" "<<param_t*opt<<"\n";

	for(int i = index_nc ; i >=0; i--)
	{
		//cout<<"i: "<<i<<"=="<<path[i]<<"\n";
		if((dista[nc]-dista[path[i]]) >= (param_t*opt))
		{
			cla = path[i];
			da = (dista[nc]-dista[path[i]]);
			break;
		}
	}

	for(int i = index_nc ; i < path.size() ; i++)
	{
		//cout<<"i: "<<i<<"=="<<path[i]<<"\n";
		if((distb[nc]-distb[path[i]]) >= (param_t*opt))
		{
			clb = path[i];
			db = (distb[nc]-distb[path[i]]);
			break;
		}
	}

	float tmp_dista[N+10],tmp_distb[N+10],ans;
	int tmp_visa[N+10],tmp_visb[N+10];

	for (int i = 0; i <= size; ++i )
	{
		tmp_dista[i] = INF;
		tmp_distb[i] = INF;
		tmp_visa[i]=tmp_visb[i]=0;
	}
	
	ans=bidijkstra_forlength( tmp_dista, tmp_distb, tmp_visa, tmp_visb, cla, clb );

	//cout<<"chlocopt"<<" "<<na<<" "<<nc<<" "<<nb<<" "<<ans<<"\n";
	//cout<<da+db<<","<<ans<<" :: "<<"nc is "<<nc<<","<<cla<<" and "<<clb<<"\n";

	if(((da+db)-ans)*((da+db)-ans)<0.0001)
		return 1;
	else
		return 0;

}

/*If a via path Pv has stretch (1 + E) and passes the T-test for T = B · dist(s, t) (with 0 < β < 1), then Pv is a B/B-E -UBS path.*/

int chstretch(vector <int> path, int index_nc, int na, int nc, int nb, float stretch)
{
	//cout<<"chstretch : "<<na<<" "<<nc<<" "<<nb<<"\n";
	int i,j,t;
	float a,b;
	a=((dista[nc]+distb[nc])/opt);
	//cout<<(dista[nc]+distb[nc])<<" "<<opt<<" "<<a<<"\n";
	b=(a-1)+((a-1)/stretch);
	//cout<<"Value is : "<<b<<"\n";
	return chlocopt(path,index_nc,na,nc,nb,b);
}