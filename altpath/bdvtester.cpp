#include <iostream>
#include <algorithm>
#include <cstdio>
#include <queue>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <limits>
#include <map>
#include <stack>
#include "random_graph.h"

using namespace std;

#define INF 1000000
#define N 15000

typedef pair< int , int> epair;

bool operator <( edge a, edge b ) 
{
	return a.weight > b.weight;
}

////////////////

map<epair, float> lengths;
map<long long int, int> vertices;

vector <vector <edge> > graph,revgraph,prevtree,foratree;
vector <int> prev,fora;

float dista[N+10],distb[N+10],opt,param_share,param_stretch,param_localt;
int visa[N+10],visb[N+10],sz,size,cnt=0;

//////////////////

int graph_print(vector <vector <edge> > graph, int size)
{
	int i,j,t;
	for(i=0;i<=size;i++)
    {
    	cout<<i<<":";
    	for(j=0;j<graph[i].size();j++)
    	{
    		cout<<graph[i][j].end_node<<" ";
    	}
    	if(graph[i].size())
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
void stack_dfs(int fg, int n, vector <int> optpath, vector <vector <edge> > arr, float share_amt[]);
void rem_plateau(map<int,int> &status,int n);

int main()
{
	srand (static_cast <unsigned> (time(0)));

	int m,i,j,x,y,iters;

	char c;
	cin>>c;

    cout<<"Input Size, edges : ";
    cin>>sz>>m;
    
    size=sz;
    float share,stretch,localt,threshold;
	
	share=0.95,stretch=0.25,localt=0.25;
	threshold=0.05;
    /*cout<<"Enter parameters : share, stretch, localt: ";
    cin>>share>>stretch>>localt;

    cout<<"Enter threshold for random graph: ";
    cin>>threshold;
	*/

    int sum=0,tmp=iters,v=1;

    graph.resize(sz+2);
    revgraph.resize(sz+2);
    prev.resize(sz+2);
    fora.resize(sz+2);

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

    cout<<"Actual Size is "<<v<<"\n";
    size=sz=v;
    //cin>>v;

    //undir_random_graph(graph,size,threshold);   

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
    	
	
	for(i=1;i<=size;i++)
    {
    	for(j=0;j<graph[i].size();j++)
    	{
    		printf("%d ",graph[i][j].end_node);
    	}
    	cout<<"\n";
    }
	*/

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

	alt_path(1,((4*size)/5),share,stretch,localt);

    return 0;
}

void rem_plateau(map<int,int> &status,int n)
{
	int i,j,t,tmp=n;
	//cout<<"INITIAL N IS "<<n<<"\n";
	while(n)
	{
		t=fora[n];
		int fg=0;
		for(i=0;i<prevtree[n].size();i++)
		{
			if((t==prevtree[n][i].end_node)&&(status.find(t)!=status.end()))
        	{
				//cout<<"PARTNERS IN PLATEAU IS "<<t<<"\n";
				status[t]=0;
				fg=1,n=t;
				break;
			}	
		}
		if(fg==0)
			n=0;
	}
	n=tmp;
	while(n)
	{
		t=prev[n];
		int fg=0;
		for(i=0;i<foratree[n].size();i++)
		{
			if((t==foratree[n][i].end_node)&&(status.find(t)!=status.end()))
			{
				//cout<<"PARTNERS IN PLATEAU IS "<<t<<"\n";
				status[t]=0;
				fg=1,n=t;
				break;
			}	
		}
		if(fg==0)
			n=0;
	}
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

	int iters=0;

	while ((!qa.empty())&&(!qb.empty())&&((ra+rb)<ans)) 
	{
		edge p = qa.top();
		int u = p.end_node;
		//cout<<p.end_node<<" : "<<p.weight<<" A\n";

		ra=dista[u];

		if(visa[u])
		{	
			qa.pop();
			continue;
		}

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
		}

		iters++;

		p = qb.top();
		u = p.end_node;
		//cout<<p.end_node<<" : "<<p.weight<<" B\n";
		rb=distb[u];		
		
		if(visb[u])
		{	
			qb.pop();
			continue;
		}

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

	//cout<<"THE TOTAL ITERATIONS IN LENGTH DIJKSTRA WERE: "<<iters<<"\n";
	return ans;
} 

float bidijkstra( float limit, int na, int nb )
{
	//cout<<"BREAK\n";
	float ans=INF,a,b,ra=0,rb=0,ma=0,mb=0,tmp_fora[N+10],tmp_prev[N+10];

	prev[na] = fora[nb] = dista[na] = distb[nb] = 0;
	
	if(na==nb)
		return 0;
	
	for(int i=0;i<=sz;i++)
	{
		tmp_prev[i]=tmp_fora[i]=0;
	}

	priority_queue < edge > qa, qb;
	
	qa.push( (edge) { na, 0 } );
	qb.push( (edge) { nb, 0 } );

	int iters=0;

	while ((!qa.empty())&&(!qb.empty())&&((ma<(limit*ans))&&(mb<(limit*ans))||((ra+rb)<ans))) 
	{
		edge p = qa.top();
		int u = p.end_node;
		//cout<<p.end_node<<" : "<<p.weight<<" A\n";

		if(visa[u])
		{	
			qa.pop();
			continue;
		}

		ra=dista[u];

		visa[u] = 1;
		if(visb[u])
			{
			a=dista[u]+distb[u];
			if(a<ans)
				ans=a;	
		}	
		
		for ( int i = 0; i<graph[u].size(); ++i )
		{
			int v = graph[u][i].end_node;
			if((dista[u]+graph[u][i].weight)<dista[v])
			{
				dista[ v ] = dista[ u ] + graph[u][ i ].weight;
				prev[v]=u;
				tmp_prev[v]=graph[u][i].weight;
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
		
		if(visb[u])
		{
			qb.pop();
			continue;
		}

		rb=distb[u];		
		visb[u] = 1;
		if(visa[u])
			{
			a=dista[u]+distb[u];
			if(a<ans)
				ans=a;	
		}	

		for ( int i = 0; i<revgraph[u].size(); ++i )
		{
			int v = revgraph[u][i].end_node;
			if ((distb[u]+revgraph[u][i].weight)<distb[v])
			{
				distb[ v ] = distb[ u ] + revgraph[u][ i ].weight;
				fora[v]=u;
				tmp_fora[v] = revgraph[u][i].weight;
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

		iters++;
		
		//cout<<ma<<":"<<mb<<" "<<limit*ans<<"\n"	;
		
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

		/*for(int i=1;i<=size;i++)
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

	prevtree.clear();
	foratree.clear();
	prevtree.resize(sz+2);
	foratree.resize(sz+2);

	for(int i=1;i<=sz;i++)
	{
		edge t1 = {i,tmp_prev[i]};
		prevtree[prev[i]].push_back(t1);
		edge t2 = {i,tmp_fora[i]};
		foratree[fora[i]].push_back(t2);
	}

	cout<<"THE TOTAL ITERATIONS WERE: "<<iters<<"\n";
	return ans;
} 

int alt_path(int na, int nb, float share, float stretch, float localt)
{
	int i,j,t,opv=0;
	
	float mina=INF;

	map<int,int> status;
	vector <int> candidates, optpath;
	float share_amt_prev[N+10],share_amt_fora[N+10];
	float tshare_amt_prev[N+10],tshare_amt_fora[N+10];

    cout<<"COOL\n";

	for ( i = 0; i <= size; ++i )
	{
		dista[i] = INF;
		distb[i] = INF;
		share_amt_prev[i]=share_amt_fora[i]=0;
		tshare_amt_prev[i]=tshare_amt_fora[i]=0;
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
			status.insert(make_pair(i,1));
			if((dista[i]+distb[i])<mina)
			{
				mina=dista[i]+distb[i];
				opv=i;
			}
		}
	}

	cout<<"The shortest between "<<na<<" and "<<nb<<" is "<< opt<<"\n";
	cout<<"It goes through "<<opv<<"\n";

	/*	for(int i=1;i<=size;i++)
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

	//graph_print(prevtree,sz);

	//graph_print(foratree,sz);

	findshare(optpath,share_amt_prev,share_amt_fora,na,nb);
		
	/*
		for(int i=1;i<=size;i++)
		{
			printf("%d:%d, ",i,(int)share_amt_prev[i]);
		}
		printf("\tThis was share a\n");
		for(int i=1;i<=size;i++)
		{
			printf("%d:%d, ",i,(int)share_amt_fora[i]);
		}
		printf("\tThis was share b\n");
	*/	

	cout<<"COOL5\n";
	
	int cands=0;

	for(i=0;i<candidates.size();i++)
	{
		j=candidates[i];
		if((status[j]==1)&&((dista[j]+distb[j])<=((1+stretch)*opt)))
		{
			cands++;
			vector <int> pathacb;
			int index_nc=findpath(pathacb,na,j,nb);
			//cout<<"A candidate path is : "<<na<<" "<<j<<" "<<nb<<"\n";
			/*for(int y=0;y<pathacb.size();y++)
			{
				cout<<pathacb[y]<<" ";
			}	
			cout<<"\n";
			*/
			t=check_validity(optpath,pathacb,index_nc,na,j,nb,share_amt_prev,share_amt_fora,share,stretch,localt);
			if(t==1)
			{
				/*cout<<"An alternative path is : "<<na<<" "<<j<<" "<<nb<<"\n";
				for(j=0;j<pathacb.size();j++)
				{
					cout<<pathacb[j]<<" ";
				}	
				cout<<"\n";
				*/cout<<"The params are : "<<param_localt<<" "<<param_stretch<<" "<<param_share<<"\n";
			}
			//rem_plateau(status,candidates[i]);
		}
	}
	cout<<"THE TOTAL CANDIDATES WERE "<<cands<<"\n";
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

	param_share=(shr/opt);
	
	str=chstretch(pathacb,index_nc,na,nc,nb,stretch);

	//cout<<"STRETCH: "<<str<<"\n";

	loc=chlocopt(pathacb,index_nc,na,nc,nb,localt);
	//loc=1;
	//cout<<"LOCAL: "<<loc<<"\n";

	if((shr/opt)>share)
		shr=0;
	else
		shr=1;

	//cout<<"SHARE : "<<shr<<"\n";

	return shr*str*loc;

}

int findshare(vector <int> optpath, float share_amt_prev[], float share_amt_fora[], int na, int nb)
{
	int i,j,t;
	vector <int> sorted_path;
	sorted_path=optpath;
	sort(sorted_path.begin(), sorted_path.end());

	/*
	cout<<"The sorted path is : "<<"\n";
	for(int y=0;y<sorted_path.size();y++)
	{
		cout<<sorted_path[y]<<" ";
	}	
	cout<<"\n";
	*/

	share_amt_prev[0]=0;
	share_amt_fora[0]=0;
	
	cnt=0;
	stack_dfs(1,na,sorted_path,prevtree,share_amt_prev);

	//stack_dfs(1,na,sorted_path,prevtree,share_amt_prev);
	cnt=0;
	stack_dfs(0,nb,sorted_path,foratree,share_amt_fora);
	//stack_dfs(0,nb,sorted_path,foratree,share_amt_fora);
	cout<<"\n\n";
	return 0;
}

void dfs(int fg, int n, vector <int> optpath, vector <vector <edge> > arr, float share_amt[])
{
	int j,t=1;
	//cout<<n<<" ";
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

void stack_dfs(int fg, int n, vector <int> optpath, vector <vector <edge> > arr, float share_amt[])
{
	int j,t=1;
	stack <int> stk;
	stk.push(n);
	while(!stk.empty())
	{
		n=stk.top();
		//cout<<n<<" ";
		stk.pop();
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
		    stk.push(arr[n][j].end_node);
		}
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
	param_localt=a;
	//cout<<(dista[nc]+distb[nc])<<" "<<opt<<" "<<a<<"\n";
	b=(a-1)+((a-1)/stretch);
	//cout<<"Value is : "<<b<<"\n";
	param_stretch=b;
	return chlocopt(path,index_nc,na,nc,nb,b);
}