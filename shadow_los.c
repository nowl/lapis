#include "lapis.h"

enum quadrant_t
{
    NE, NW,
    EN, ES,
    SE, SW,
    WN, WS
};

static void trace(enum quadrant_t quad, float min, float max,
                  int origin_x, int origin_y, int pos, int max_pos,
                  blocked_f block_f, set_visible_f set_vis_f)
{
    //printf("tracing %.2f to %.2f pos %d\n", min, max, pos);

    int i;
    char prev_blocked = 0;

    /* first check if we should proceed */
    if(pos == max_pos)
        return;

    char finished_scan = 0;
    i = pos * min;
    while(!finished_scan)
    {
        int rx = 0, ry = 0;
        switch(quad)
        {
        case NW:
        case NE:
            rx = origin_x + i;
            ry = origin_y - pos;
            break;
        case EN:
        case ES:
            rx = origin_x + pos;
            ry = origin_y - i;
            break;
        case WN:
        case WS:
            rx = origin_x - pos;
            ry = origin_y - i;
            break;
        case SW:
        case SE:
            rx = origin_x + i;
            ry = origin_y + pos;
            break;
        };       

        if(!block_f(rx, ry))
        {
            /* not blocked */
            if(prev_blocked)
            {
                /* adjust min since we're in a blocked region */
                prev_blocked = 0;

                switch(quad)
                {
                case NW:
                case ES:
                    min = (i + 0.5) / (pos + 0.5);
                    if(min > 0) min = 0;
                    break;
                case EN:
                case NE:
                    min = (i - 0.5) / (pos + 0.5);
                    if(min < 0) min = 0;
                    break;
                case WN:
                case SE:
                    min = (i - 0.5) / (pos - 0.5);
                    if(min < 0) min = 0;
                case WS:
                case SW:
                    min = (i + 0.5) / (pos - 0.5);
                    if(min > 0) min = 0;
                };
            }
        }
        else
        {
            /* hit a blocked cell */
            if(!prev_blocked)
            {
                if(i != pos*min)
                {
                    /* send off recursive trace with new max if
                     * not the first cell */
                    float new_max = 0;
                    switch(quad)
                    {
                    case NW:
                    case ES:
                        new_max = (i - 0.5) / (pos - 0.5);
                        break;
                    case NE:
                    case EN:
                        new_max = (i + 0.5) / (pos - 0.5);
                        break;
                    case WN:
                    case SE:
                        new_max = (i + 0.5) / (pos + 0.5);
                        break;
                    case WS:
                    case SW:
                        new_max = (i - 0.5) / (pos + 0.5);
                        break;
                    };

                    trace(quad, min, new_max,
                          origin_x, origin_y, pos + 1, max_pos,
                          block_f, set_vis_f);
                }
               
                prev_blocked = 1;
            }
        }

        /* set visiblity for this cell */
        set_vis_f(rx, ry);
       
        switch(quad)
        {
        case NW:
        case ES:
        case WS:
        case SW:
            i++;
            if(i > pos*max)
                finished_scan = 1;
            break;
        case NE:
        case EN:
        case WN:
        case SE:
            i--;
            if(i < pos*max)
                finished_scan = 1;
            break;
        };
    }

    /* hit the end of this scan, recursively trace with new min and
     * max unless last cell was blocked */
    if(!prev_blocked)
        trace(quad, min, max,
              origin_x, origin_y, pos + 1, max_pos,
              block_f, set_vis_f);
}

void los_visibility(int origin_x, int origin_y, int depth, blocked_f get_blocked, set_visible_f set_vis)
{
    set_vis(origin_x, origin_y);
    
    trace(NE, 1, 0, origin_x, origin_y, 1, depth, get_blocked, set_vis);
    trace(NW, -1, 0, origin_x, origin_y, 1, depth, get_blocked, set_vis);
    trace(EN, 1, 0, origin_x, origin_y, 1, depth, get_blocked, set_vis);
    trace(ES, -1, 0, origin_x, origin_y, 1, depth, get_blocked, set_vis);
    trace(WN, 1, 0, origin_x, origin_y, 1, depth, get_blocked, set_vis);
    trace(WS, -1, 0, origin_x, origin_y, 1, depth, get_blocked, set_vis);
    trace(SE, 1, 0, origin_x, origin_y, 1, depth, get_blocked, set_vis);
    trace(SW, -1, 0, origin_x, origin_y, 1, depth, get_blocked, set_vis);
}
