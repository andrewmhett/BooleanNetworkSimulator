import sys
sys.path.append("/Library/Python/3.7/site-packages")
import pygame
import pygame.gfxdraw
import asyncio
import math
pygame.display.init()
width=1000
height=800
scale_factor=1.6
x_offset=0
y_offset=0
framerate=144
pygame.display.set_mode((width,height),pygame.RESIZABLE)
screen=pygame.display.get_surface()

and_surf=pygame.image.load("assets/AND.png")
or_surf=pygame.image.load("assets/OR.png")
not_surf=pygame.image.load("assets/NOT.png")
nand_surf=pygame.image.load("assets/NAND.png")
nor_surf=pygame.image.load("assets/NOR.png")
button_up_surf=pygame.Surface((int(height/7),int(1.66*(height/7))))
button_down_surf=pygame.Surface((int(height/7),int(1.66*(height/7))))
pygame.draw.rect(button_up_surf,(0,0,0),pygame.Rect(int(height/28)-10,int(1.66*(height/28))-10,int(height/14)+20,int(1.66*(height/14))+20),4)
pygame.gfxdraw.box(button_down_surf,pygame.Rect(int(height/28)-10,int(1.66*(height/28))-10,int(height/14)+20,int(1.66*(height/14))+20),(0,0,0,255))
high_bit_surf=pygame.image.load("assets/ONE.png")

components=[]
conn_points=[]
connections=[]

class orientation:
    HORIZONTAL=1

class IO:
    INPUT=1
    OUTPUT=2

class ConnectionPoint:
    global conn_points
    def __init__(self,rel_x,rel_y,io,parent):
        self.rel_x=rel_x
        self.rel_y=rel_y
        self.io=io
        self.parent_obj=parent
        self.state=False
        conn_points.append(self)
    def draw(self):
       center=(int((self.parent_obj.x+self.rel_x)/scale_factor)+x_offset,int((self.parent_obj.y+self.rel_y)/scale_factor)+y_offset)
       color=(0,0,0) if (self.state==False) else (0,255,0)
       pygame.draw.circle(screen,color,center,self.parent_obj.height/(12*scale_factor))

class Connection:
    global connections
    def __init__(self,p1,p2):
        self.p1=p1
        self.p2=p2
        self.state=False
        connections.append(self)
    def draw(self):
        p1=self.p1
        p2=self.p2
        p1_pos=((p1.parent_obj.x+p1.rel_x)/scale_factor)+x_offset,((p1.parent_obj.y+p1.rel_y)/scale_factor)+y_offset
        p2_pos=((p2.parent_obj.x+p2.rel_x)/scale_factor)+x_offset,((p2.parent_obj.y+p2.rel_y)/scale_factor)+y_offset
        color=((0,0,0) if self.state == False else (0,255,0))
        pygame.draw.aaline(screen,color,p1_pos,p2_pos,int(3*scale_factor))

class Menu:
    def __init__(self,orientation,x,y,width,height):
        self.orientation=orientation
        self.x=x
        self.y=y
        self.width=width
        self.height=height
        self.components=[]
    def draw(self):
        counter=0
        for component in self.components:
            component.draw(counter)
            counter+=1
    def add_component(self,component):
        self.components.append(component)
    def get_component_collision(self,x,y):
        counter=-1
        for component in self.components:
            try:
                pos_offset=int(component.width*len(self.components)/2)
            except ZeroDivisionError:
                pos_offset=0
            if pos_offset%2==0:
                pos_offset-=component.width/2
            counter+=1
            comp_x=self.x-pos_offset+(counter*(component.width))
            comp_y=self.y
            if comp_x-int(component.width/2) < x and comp_x+int(component.width/2) > x:
                if comp_y-int(component.height/2) < y and comp_y+int(component.height/2) > y:   
                    return component
        return None

class MenuComponent():
    def __init__(self,menu,surf,callback):
        self.menu=menu
        self.callback=callback
        self.height=int(height/15)
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.smoothscale(surf,(self.width,self.height))
    def draw(self,position):
        if self.menu.orientation == orientation.HORIZONTAL:
            try:
                pos_offset=int(self.width*len(self.menu.components)/2)
            except ZeroDivisionError:
                pos_offset=0
            if pos_offset%2==0:
                pos_offset-=int(self.width/2)
            if self.menu.x-pos_offset+(position*(self.width))-int(self.width/2)>0:
                screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(self.menu.x-pos_offset+(position*(self.width))-int(self.width/2),self.menu.y-int(self.height/2),self.width,self.height)))
            pygame.draw.rect(screen,(0,0,0),pygame.Rect(self.menu.x-pos_offset+(position*(self.width))-int(self.width/2),self.menu.y-int(self.height/2),self.width,self.height),3)

class Component:
    def __init__(self,x,y,inputs,outputs):
        global components
        self.inputs=inputs
        self.outputs=outputs
        self.x=x
        self.y=y
        components.append(self)
    def draw_children(self):
        for child in self.inputs: 
            child.draw()
        for child in self.outputs:
            child.draw()
    def destroy(self):
        global connections
        global components
        global conn_points
        inputs=self.inputs
        outputs=self.outputs
        conns=[]
        for connection in connections:
            conns.append(connection)
        for connection in conns:
            if connection.p1 in inputs or connection.p1 in outputs or connection.p2 in inputs or connection.p2 in outputs:
                connection.state=False
                connection.p1.state=False
                connection.p2.state=False
                connections.remove(connection)
        for connector in self.inputs:
            conn_points.remove(connector)
        for connector in self.outputs:
            connector.state=False
            conn_points.remove(connector)
        components.remove(self)

class AND(Component):
    def __init__(self,x,y):
        self.height=int(height/6)
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.smoothscale(and_surf,(self.width,self.height))
        inputs=[
            ConnectionPoint(-self.width/2,-self.height/4,IO.INPUT,self),
            ConnectionPoint(-self.width/2,self.height/4,IO.INPUT,self)
        ]
        outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        super().__init__(x,y,inputs,outputs)
    def draw(self):
        super().draw_children()
        self.image=pygame.transform.smoothscale(and_surf,(int(self.width/scale_factor),int(self.height/scale_factor)))
        if int((self.x-(self.width/2))/scale_factor)+x_offset>0:
            screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor))))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor)),3)
    def dump(self):
        return({
                "type":AND,
                "x":self.x,
                "y":self.y
                })

class NAND(Component):
    def __init__(self,x,y):
        self.height=int(height/6)
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.smoothscale(nand_surf,(self.width,self.height))
        inputs=[
            ConnectionPoint(-self.width/2,-self.height/4,IO.INPUT,self),
            ConnectionPoint(-self.width/2,self.height/4,IO.INPUT,self)
        ]
        outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        super().__init__(x,y,inputs,outputs)
    def draw(self):
        super().draw_children()
        self.image=pygame.transform.smoothscale(nand_surf,(int(self.width/scale_factor),int(self.height/scale_factor)))
        if int((self.x-(self.width/2))/scale_factor)+x_offset>0:
            screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor))))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor)),3)

class OR(Component):
    def __init__(self,x,y):
        self.height=int(height/6)
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.smoothscale(or_surf,(self.width,self.height))
        inputs=[
            ConnectionPoint(-self.width/2,-self.height/4,IO.INPUT,self),
            ConnectionPoint(-self.width/2,self.height/4,IO.INPUT,self)
        ]
        outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        super().__init__(x,y,inputs,outputs)
    def draw(self):
        super().draw_children()
        self.image=pygame.transform.smoothscale(or_surf,(int(self.width/scale_factor),int(self.height/scale_factor)))
        if int((self.x-(self.width/2))/scale_factor)+x_offset>0:
            screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor))))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor)),3)

class NOR(Component):
    def __init__(self,x,y):
        self.height=int(height/6)
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.smoothscale(nor_surf,(self.width,self.height))
        inputs=[
            ConnectionPoint(-self.width/2,-self.height/4,IO.INPUT,self),
            ConnectionPoint(-self.width/2,self.height/4,IO.INPUT,self)
        ]
        outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        super().__init__(x,y,inputs,outputs)
    def draw(self):
        super().draw_children()
        self.image=pygame.transform.smoothscale(nor_surf,(int(self.width/scale_factor),int(self.height/scale_factor)))
        if int((self.x-(self.width/2))/scale_factor)+x_offset>0:
            screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor))))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor)),3)

class NOT(Component):
    def __init__(self,x,y):
        self.height=int(height/6)
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.smoothscale(not_surf,(self.width,self.height))
        inputs=[
            ConnectionPoint(-self.width/2,0,IO.INPUT,self)
        ]
        outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        super().__init__(x,y,inputs,outputs)
    def draw(self):
        super().draw_children()
        self.image=pygame.transform.smoothscale(not_surf,(int(self.width/scale_factor),int(self.height/scale_factor)))
        if int((self.x-(self.width/2))/scale_factor)+x_offset>0:
            screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor))))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor)),3)

class BUTTON(Component):
    def __init__(self,x,y):
        self.height=int(height/6)
        self.width=int(1.66*(self.height))
        self.pressed=False
        self.image=pygame.transform.smoothscale(button_up_surf,(self.width,self.height))
        inputs=[
            ConnectionPoint(-self.width/2,0,IO.INPUT,self)
        ]
        outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        super().__init__(x,y,inputs,outputs)
    def draw(self):
        super().draw_children()
        if self.pressed:
            self.image=pygame.transform.smoothscale(button_down_surf,(int(self.width/scale_factor),int(self.height/scale_factor)))
        else:
            self.image=pygame.transform.smoothscale(button_up_surf,(int(self.width/scale_factor),int(self.height/scale_factor)))
        if int((self.x-(self.width/2))/scale_factor)+x_offset>0:
            screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor))))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor)),3)

class HIGH_BIT(Component):
    def __init__(self,x,y):
        self.height=int(height/6)
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.smoothscale(high_bit_surf,(self.width,self.height))
        inputs=[]
        outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        outputs[0].state=True
        super().__init__(x,y,inputs,outputs)
    def draw(self):
        super().draw_children()
        self.image=pygame.transform.smoothscale(high_bit_surf,(int(self.width/scale_factor),int(self.height/scale_factor)))
        if int((self.x-(self.width/2))/scale_factor)+x_offset>0:
            screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor))))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor)),3)

def get_component_collision(x,y):
    for i in range(len(components)):
        component=components[len(components)-i-1]
        if ((component.x+(component.width)/2)/scale_factor)+x_offset > x and ((component.x-(component.width/2))/scale_factor)+x_offset < x:
            if ((component.y+(component.height)/2)/scale_factor)+y_offset > y and ((component.y-(component.height/2))/scale_factor)+y_offset < y:
                return component
    return None

def get_connector_collision(x,y):
    if get_component_collision(x,y) == None:
        for connector in conn_points:
            if math.sqrt((((connector.parent_obj.x)/scale_factor+x_offset+(connector.rel_x)/scale_factor-x)**2)+(((connector.parent_obj.y)/scale_factor+y_offset+(connector.rel_y)/scale_factor)-y)**2)<(30/scale_factor):
                return connector
    return None

last_connector=None
component_menu=None

def redraw():
    global component_menu
    global components
    w,h=width,height
    screen.fill((255,255,255))
    if last_connector != None:
        pygame.draw.aaline(screen,(0,0,0),(int((last_connector.parent_obj.x+last_connector.rel_x)/scale_factor)+x_offset,int((last_connector.parent_obj.y+last_connector.rel_y)/scale_factor)+y_offset),(int(pygame.mouse.get_pos()[0]),int(pygame.mouse.get_pos()[1])),int(2*scale_factor))
    for connection in connections:
        connection.draw()
    for component in components:
        component.draw()
    pygame.gfxdraw.box(screen,pygame.Rect(0,0,w,h/12),(0,0,0,100))
    component_menu=Menu(orientation.HORIZONTAL,w/2,h/24,w,h/4)
    component_menu_2=Menu(orientation.HORIZONTAL,w/2,(3*h)/24,w,h/4)
    component_menu.add_component(MenuComponent(component_menu,and_surf,lambda:(AND(int(w/2),int(h/2)).draw())))
    component_menu.add_component(MenuComponent(component_menu,or_surf,lambda:(OR(int(w/2),int(h/2)).draw())))
    component_menu.add_component(MenuComponent(component_menu,not_surf,lambda:(NOT(int(w/2),int(h/2)).draw())))
    component_menu.add_component(MenuComponent(component_menu,nor_surf,lambda:(NOR(int(w/2),int(h/2)).draw())))
    component_menu.add_component(MenuComponent(component_menu,nand_surf,lambda:(NAND(int(w/2),int(h/2)).draw())))
    component_menu.add_component(MenuComponent(component_menu,button_up_surf,lambda:(BUTTON(int(w/2),int(h/2)).draw())))
    component_menu.add_component(MenuComponent(component_menu,high_bit_surf,lambda:(HIGH_BIT(int(w/2),int(h/2)).draw())))
    component_menu.draw()
    pygame.display.update()

last_x=0
last_y=0
drag=None
moved=False

async def update_loop():
    global last_x
    global last_y
    global width
    global height
    global drag
    global last_connector
    global moved
    global scale_factor
    global x_offset
    global y_offset
    while True:
        pygame.event.pump()
        for event in pygame.event.get():
            try:
                if event.type==pygame.VIDEORESIZE:
                    width, height = event.dict['size']
                    pygame.display.set_mode((width,height),pygame.RESIZABLE)
                if event.type==pygame.MOUSEBUTTONDOWN:
                    if event.button == 4:
                        scale_factor=scale_factor*1.1
                    if event.button == 5:
                        scale_factor=scale_factor/1.1
                    if pygame.mouse.get_pressed()[0]:
                        if component_menu.get_component_collision(event.pos[0],event.pos[1]) != None:
                            component=component_menu.get_component_collision(event.pos[0],event.pos[1])
                            component.callback()
                        if get_connector_collision(event.pos[0],event.pos[1]) != None:
                            connector=get_connector_collision(event.pos[0],event.pos[1])
                            if last_connector != None:
                                if last_connector.parent_obj != connector.parent_obj:
                                        Connection(last_connector,connector)
                                        last_connector=None
                            else:
                                last_connector=connector
                        else:
                            last_connector=None
                    if pygame.mouse.get_pressed()[2]:
                        if get_component_collision(event.pos[0],event.pos[1]) != None:
                            component=get_component_collision(event.pos[0],event.pos[1])
                            component.destroy()
                if event.type==pygame.MOUSEBUTTONUP:
                    if pygame.mouse.get_pressed(0):
                        if not moved:
                            if get_component_collision(event.pos[0],event.pos[1]) != None:
                                component=get_component_collision(event.pos[0],event.pos[1])
                                if isinstance(component,BUTTON):
                                    if not component.pressed:
                                        component.pressed=True
                                    else:
                                        component.pressed=False
                        else:
                            moved=False
                if pygame.mouse.get_pressed()[0]:
                    delta_x=event.pos[0]-last_x
                    delta_y=event.pos[1]-last_y
                    if last_x != 0:
                        if drag==None:
                            if get_component_collision(event.pos[0],event.pos[1]) != None:
                                component=get_component_collision(event.pos[0],event.pos[1])
                                drag=component
                                moved=True
                            else:
                                x_offset+=delta_x
                                y_offset+=delta_y
                        else:
                            drag.x+=delta_x*scale_factor
                            drag.y+=delta_y*scale_factor
                    last_x=event.pos[0]
                    last_y=event.pos[1]
                if event.type==pygame.MOUSEBUTTONUP and event.button==1:
                    drag=None
                    last_x=0
                    last_y=0
            except AttributeError:
                pass
        await asyncio.sleep(1/framerate)

async def draw_loop():
    while True:
        redraw()
        await asyncio.sleep(1/framerate)
