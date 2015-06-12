#include<iostream>
#include<algorithm>
#include<cstdio>
#include<vector>
#include<cstdlib>
#include<ctime>
#include<limits>

using namespace std;

struct edge{
    int start_node;
    int end_node;
    float weight;
};

typedef struct edge edge;

vector <vector <edge> > gr;
vector <int> present_state, next_state;

float dist[210][210];
int nextv[210][210];
int backv[210][210];

int floyd_warshall(vector <vector <edge> > gr, float dist[][210], int sz);
int alt_path(vector <vector <edge> > gr, int na, int nb, int sz, float share, float stretch, float localt);
int check_validity(vector <vector <edge> > gr, int na, int nc, int nb, int sz, float share, float stretch, float localt);
int findpath(vector <vector <edge> > gr, vector <int> &path, int nextv[][210], int na, int nc, int sz);
float chshare(vector <int> patha, vector <int> pathb, int sz);
int chlocopt(vector <vector <edge> > gr, int na, int nc, int nb, int sz, float param_t);
int chstretch(vector <vector <edge> > gr, int na, int nc, int nb, int sz, float stretch);

int main()
{
	srand (static_cast <unsigned> (time(0)));

	int size,m,i,j,x,y,iters;
    
    cout<<"Input Size, Edges: ";
    cin>>size>>m;

    float share,stretch,localt;

    cout<<"Enter parameters : ";
    cin>>share>>stretch>>localt;

    int sum=0,tmp=iters;

    gr.resize(size+2);

    for(i=0;i<m;i++)
    {
    	int x,y;
    	float a;
        scanf("%d %d %f",&x,&y,&a);
        edge t;
        t.start_node=x;
        t.end_node=y;
        t.weight=a;
        gr[x].push_back(t);
        t.start_node=y;
        t.end_node=x;
        t.weight=a;
        gr[y].push_back(t);
    }
	
	floyd_warshall(gr,dist,size);
	alt_path(gr,20,19,size,share,stretch,localt);

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
			dist[i][j]=numeric_limits<float>::max();
			nextv[i][j]=0;
			backv[i][j]=0;
		}
	}

	for(i=1;i<=sz;i++)
	{
		dist[i][i]=0;
		nextv[i][i]=0;
		for(j=0;j<gr[i].size();j++)
		{
			dist[i][gr[i][j].end_node]=gr[i][j].weight;
			nextv[i][gr[i][j].end_node]=gr[i][j].end_node;
			backv[i][gr[i][j].end_node]=i;
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
					nextv[i][j]=nextv[i][k];
					backv[i][backv[i][k]]=backv[i][j];
				}
			}
		}
	}

	return 0;
}

int alt_path(vector <vector <edge> > gr, int na, int nb, int sz, float share, float stretch, float localt)
{
	int i,j,t;
	
	for(i=1;i<=sz;i++)
	{
		if((i!=na)&&(i!=nb)&&((dist[na][i]+dist[i][nb])<=((1+stretch)*dist[na][nb])))
		{
			t=check_validity(gr,na,i,nb,sz,share,stretch,localt);
			if(t==1)
			{
				cout<<"An alternative path is : "<<na<<" "<<i<<" "<<nb<<"\n";
				vector <int> pathacb;
				findpath(gr,pathacb,nextv,na,i,sz);
				findpath(gr,pathacb,nextv,i,nb,sz);
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

int findpath(vector <vector <edge> > gr, vector <int> &path, int nextv[][210], int na, int nc, int sz)
{
	int i,j,t;
	while(na!=0)
	{
		path.push_back(na);
		na=nextv[na][nc];
	}
	return 0;
}

int check_validity(vector <vector <edge> > gr, int na, int nc, int nb, int sz, float share, float stretch, float localt)
{
	int i,j,t;
	float shr,str,loc;

	vector <int> pathacb, pathab;

	findpath(gr,pathab,nextv,na,nb,sz);
	findpath(gr,pathacb,nextv,na,nc,sz);
	findpath(gr,pathacb,nextv,nc,nb,sz);
	
	shr=chshare(pathab,pathacb,sz);
	
	//cout<<"SHARE : "<<shr<<"\n";

	str=chstretch(gr,na,nc,nb,sz,stretch);

	//cout<<"STRETCH: "<<str<<"\n";

	loc=chlocopt(gr,na,nc,nb,sz,localt);

	//cout<<"LOCAL: "<<loc<<"\n";

	if((shr/dist[na][nb])>share)
		shr=0;
	else
		shr=1;

	return shr*str*loc;

}

float chshare(vector <int> patha, vector <int> pathb, int sz)
{
	int i,j,t=0;
	int stata[210],statb[210];
	for(i=0;i<=sz;i++)
	{
		stata[i]=statb[i]=0;
	}

	for(i=0;i<patha.size();i++)
	{
		stata[patha[i]]=i+1;
	}
	for(i=0;i<pathb.size();i++)
	{
		statb[pathb[i]]=i+1;		
	}

	float shr=0;
	for(i=0;i<patha.size();i++)
	{
		if(stata[patha[i]]*statb[patha[i]])
		{
			if(((i+1)<patha.size())&&(patha[i+1]==pathb[statb[patha[i]]-1]))
			{
				shr+=dist[patha[i]][patha[i+1]];
			}
		}
	}
	return shr;
}

int chlocopt(vector <vector <edge> > gr, int na, int nc, int nb, int sz, float param_t)
{
	vector<int> pathatc;

	findpath(gr, pathatc, nextv, na, nc, sz);
	int closestptatc = na;
	float distpttc = dist[na][nc];

	for(int i = 0 ; i < pathatc.size(); i++)
	{
		if(dist[pathatc[i]][nc] > param_t*dist[na][nb])
		{
			closestptatc = pathatc[i];
			distpttc = dist[pathatc[i]][nc];
		}
	}

	vector<int> pathctob;
	findpath(gr, pathctob, nextv, nc, nb, sz);
	int closestptctb = nb;
	float distpttc2 = dist[nc][nb];
	for(int i = pathctob.size() - 1; i >= 0; i--)
	{
		if(dist[pathctob[i]][nc] > param_t*dist[na][nb])
		{
			closestptctb = pathctob[i];
			distpttc2 = dist[pathctob[i]][nc];
		}
	}

	if(distpttc + distpttc2 == dist[closestptctb][closestptatc])
		return 1;
	else
		return 0;
}

/*If a via path Pv has stretch (1 + E) and passes the T-test for T = B · dist(s, t) (with 0 < β < 1), then Pv is a B/B-E -UBS path.*/

int chstretch(vector <vector <edge> > gr, int na, int nc, int nb, int sz, float stretch)
{
	//cout<<"chstretch\n";
	int i,j,t;
	float a,b;
	a=((dist[na][nc]+dist[nc][nb])/dist[na][nb]);
	//cout<<a<<"\n";
	b=(a-1)+((a-1)/stretch);
	//cout<<"Value is : "<<b<<"\n";
	return chlocopt(gr,na,nc,nb,sz,b);
}