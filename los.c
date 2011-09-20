#include "lapis.h"

#include <math.h>

#define DIAMETER 30
#define PI       3.1415926

struct los
{
    float default_mina, default_maxa;
    float mina, maxa;
    float light;
};

static struct los LOSData[DIAMETER*DIAMETER];
static int Queue[DIAMETER*DIAMETER];
static int QueueStart = 0;
static int QueueEnd = 0;

static float find_min_angle(int x, int y)
{
    if(x > 0 && y > 0)
        return atan( (y-0.4)/(x+0.4) );
    else if(x == 0 && y > 0)
        return atan( (y-0.4)/0.4 );
    else if(x < 0 && y > 0)
        return PI - atan( -(y+0.4)/(x+0.4) );
    else if(x < 0 && y == 0)
        return PI - atan( -0.4/(x+0.4) );
    else if(x < 0 && y < 0)
        return PI + atan( (y+0.4)/(x-0.4) );
    else if(x == 0 && y < 0)
        return PI + atan( -(y+0.4)/0.4 );
    else if(x > 0 && y < 0)
        return 2*PI + atan( (y-0.4)/(x-0.4) );
    else if(x > 0 && y == 0)
        return 2*PI + atan( -0.4/(x-0.4) );

    return 0;
}

static float find_max_angle(int x, int y)
{
    if(x > 0 && y > 0)
        return atan( (y+0.4)/(x-0.4) );
    else if(x == 0 && y > 0)
        return PI - atan( (y-0.4)/0.4 );
    else if(x < 0 && y > 0)
        return PI - atan( -(y-0.4)/(x-0.4) );
    else if(x < 0 && y == 0)
        return PI + atan( -0.4/(x+0.4) );
    else if(x < 0 && y < 0)
        return PI + atan( (y-0.4)/(x+0.4) );
    else if(x == 0 && y < 0)
        return 2*PI-atan( -(y+0.4)/0.4 );
    else if(x > 0 && y < 0)
        return 2*PI + atan( (y+0.4)/(x+0.4) );
    else if(x > 0 && y == 0)
        return atan( 0.4/(x-0.4) );

    return 0;
}

static int x_to_rx(int x) { return x - DIAMETER/2; }
static int y_to_ry(int y) { return y - DIAMETER/2; }
static int rxy_to_i(int x, int y) { return (y+DIAMETER/2) * DIAMETER + (x+DIAMETER/2); }
static int xy_to_i(int x, int y) { return y * DIAMETER + x; }
static int i_to_x(int i) { return i - i/DIAMETER*DIAMETER; }
static int i_to_y(int i) { return i/DIAMETER; }

static void precalc_angles()
{
    int x, y;

    for(y=0; y<DIAMETER; y++)
    {
        for(x=0; x<DIAMETER; x++)
        {
            int rx = x_to_rx(x);
            int ry = y_to_ry(y);

            struct los *los = &LOSData[y*DIAMETER+x];

            los->default_mina = find_min_angle(rx, ry);
            los->default_maxa = find_max_angle(rx, ry);
        }
    }
}

void los_init()
{
    precalc_angles();
}

static void reset_los()
{
    int i;
    for(i=0; i<DIAMETER*DIAMETER; i++)
    {
        LOSData[i].light = 0;
        LOSData[i].mina = LOSData[i].default_mina;
        LOSData[i].maxa = LOSData[i].default_maxa;
    }

    /* set origin to lit for completeness */
    LOSData[rxy_to_i(0, 0)].light = 1.0;

}

static void add_light_to(int rx, int ry, float light, float mina, float maxa)
{
    int i;

    //printf("  trying to add %d, %d\n", rx, ry);


    /* outside bounds? */
    if(rx < -DIAMETER/2 ||
       rx >= DIAMETER/2 ||
       ry < -DIAMETER/2 ||
       ry >= DIAMETER/2)
        return;

    /* first check if it's in the queue already */
    for(i=QueueStart; i<QueueEnd; i++)
    {
        int ind = Queue[i];
        if(rx == x_to_rx(i_to_x(ind)) && ry == y_to_ry(i_to_y(ind)))
        {
            LOSData[ind].light = light;
          
            /* adjust angles */
            if(mina < LOSData[ind].mina) LOSData[ind].mina = mina;
            if(maxa > LOSData[ind].maxa) LOSData[ind].maxa = maxa;

          
            return;
        }
    }

    /* otherwise add to the queue */
    i = rxy_to_i(rx, ry);
    Queue[QueueEnd] = i;
    LOSData[i].light = light;

    /* adjust angles */
    if(LOSData[i].mina < mina) LOSData[i].mina = mina;
    if(LOSData[i].maxa > maxa) LOSData[i].maxa = maxa;

    QueueEnd = (QueueEnd + 1) % (DIAMETER*DIAMETER);

    //printf("  added %d, %d, QueueEnd = %d\n", rx, ry, QueueEnd);

}

static void add_light(int rx, int ry, float light, float opaqueness, float mina, float maxa)
{
    /* adjust queue by transferring light and/or adding new unseen blocks */
    float light_transferal = light * (1 - opaqueness);

    /* bailout if not enough light to pass on */
    if(light_transferal < 0.1)
        return;

    //printf("%d, %d, QueueStart = %d\n", rx, ry, QueueStart);

    if(rx > 0 && ry > 0)
    {
        add_light_to(rx+1, ry, light_transferal, mina, maxa);
        add_light_to(rx, ry+1, light_transferal, mina, maxa);
    }
    else if(rx == 0 && ry > 0)
    {
        add_light_to(rx+1, ry, light_transferal, mina, maxa);
        add_light_to(rx, ry+1, light_transferal, mina, maxa);
        add_light_to(rx-1, ry, light_transferal, mina, maxa);
    }
    else if(rx < 0 && ry > 0)
    {
        add_light_to(rx, ry+1, light_transferal, mina, maxa);
        add_light_to(rx-1, ry, light_transferal, mina, maxa);
    }
    else if(rx < 0 && ry == 0)
    {
        add_light_to(rx, ry+1, light_transferal, mina, maxa);
        add_light_to(rx-1, ry, light_transferal, mina, maxa);
        add_light_to(rx, ry-1, light_transferal, mina, maxa);
    }
    else if(rx < 0 && ry < 0)
    {
        add_light_to(rx-1, ry, light_transferal, mina, maxa);
        add_light_to(rx, ry-1, light_transferal, mina, maxa);
    }
    else if(rx == 0 && ry < 0)
    {
        add_light_to(rx-1, ry, light_transferal, mina, maxa);
        add_light_to(rx, ry-1, light_transferal, mina, maxa);
        add_light_to(rx+1, ry, light_transferal, mina, maxa);
    }
    else if(rx > 0 && ry < 0)
    {
        add_light_to(rx, ry-1, light_transferal, mina, maxa);
        add_light_to(rx+1, ry, light_transferal, mina, maxa);
    }
    else if(rx > 0 && ry == 0)
    {
        add_light_to(rx, ry-1, light_transferal, mina, maxa);
        add_light_to(rx+1, ry, light_transferal, mina, maxa);
        add_light_to(rx, ry+1, light_transferal, mina, maxa);
    }
}

void los_run(int origin_x, int origin_y, opaque_f opfun, set_los_f visibility)
{
    int i, j;

    reset_los();
   
    /* add initial E,N,W,S */
    Queue[0] = rxy_to_i(1, 0);
    LOSData[Queue[0]].light = 1-opfun(origin_x + 1, origin_y + 0);
    Queue[1] = rxy_to_i(0, 1);
    LOSData[Queue[1]].light = 1-opfun(origin_x + 0, origin_y + 1);
    Queue[2] = rxy_to_i(-1, 0);
    LOSData[Queue[2]].light = 1-opfun(origin_x - 1, origin_y + 0);
    Queue[3] = rxy_to_i(0, -1);
    LOSData[Queue[3]].light = 1-opfun(origin_x + 0, origin_y - 1);
    QueueStart = 0;
    QueueEnd = 4;

    while(QueueEnd != QueueStart)
    {
        int new = Queue[QueueStart];

        QueueStart = (QueueStart + 1) % (DIAMETER*DIAMETER);

       
        int y = i_to_y(new);
        int x = i_to_x(new);
        int ry = y_to_ry(y);
        int rx = x_to_rx(x);

        /* if angle decreases to the point where it should not be
         * visible then move to the next element in the queue */
        if(LOSData[new].maxa > 0 && LOSData[new].maxa < PI/2 &&
           LOSData[new].mina > 3*PI/2 && LOSData[new].mina < 2*PI)
        {
            /* check for 360 degree overflow */
            if(LOSData[new].mina > LOSData[new].maxa + 2*PI)
                continue;
        }
        else
        {
            if(LOSData[new].mina > LOSData[new].maxa)
                continue;

        }

        float mina = LOSData[xy_to_i(x, y)].mina;
        float maxa = LOSData[xy_to_i(x, y)].maxa;
        float light = LOSData[xy_to_i(x, y)].light;
      
        float opaqueness = opfun(origin_x + rx, origin_y + ry);
        add_light(rx, ry, light, opaqueness, mina, maxa);

        //printf("rx = %d, ry = %d, %f, %f\n", rx, ry, light, opaqueness);
    }

    /* pass data to visibility function */
    for(j=0; j<DIAMETER; j++)
    {
        for(i=0; i<DIAMETER; i++)
            visibility(x_to_rx(i), y_to_ry(j),
                       LOSData[xy_to_i(i, j)].light);
    }
}
