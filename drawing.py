import sys
sys.path.append("/Library/Python/3.7/site-packages")
import pygame
import pygame.gfxdraw
import asyncio
import math
pygame.display.init()
width=800
height=800
pygame.display.set_mode((width,height),pygame.RESIZABLE)
screen=pygame.display.get_surface()

and_surf=pygame.image.load("assets/AND.png")
or_surf=pygame.image.load("assets/OR.png")
not_surf=pygame.image.load("assets/NOT.png")
nand_surf=pygame.image.load("assets/NAND.png")
nor_surf=pygame.image.load("assets/NOR.png")

components=[]
conn_points=[]
connections=[]

class orientation:
    HORIZONTAL=1

class ConnectionPoint:
    global conn_points
    def __init__(self,rel_x,rel_y,parent):
        self.rel_x=rel_x
        self.rel_y=rel_y
        self.parent_obj=parent
        conn_points.append(self)
    def draw(self):
       x=self.parent_obj.x+self.rel_x
       y=self.parent_obj.y+self.rel_y
       center=(int(self.parent_obj.x+self.rel_x),int(self.parent_obj.y+self.rel_y))
       pygame.draw.circle(screen,(0,0,0),center,self.parent_obj.height/25)

class Connection:
    global connections
    def __init__(self,p1,p2):
        self.p1=p1
        self.p2=p2
        connections.append(self)
    def draw(self):
        p1=self.p1
        p2=self.p2
        p1_pos=(p1.parent_obj.x+p1.rel_x,p1.parent_obj.y+p1.rel_y)
        p2_pos=(p2.parent_obj.x+p2.rel_x,p2.parent_obj.y+p2.rel_y)
        pygame.draw.line(screen,(0,0,0),p1_pos,p2_pos,3)

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
                pos_offset=component.width*int(len(self.components)/2)
            except ZeroDivisionError:
                pos_offset=0
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
        self.height=int(height/10)
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.smoothscale(surf,(self.width,self.height))
    def draw(self,position):
        if self.menu.orientation == orientation.HORIZONTAL:
            try:
                pos_offset=self.width*int(len(self.menu.components)/2)
            except ZeroDivisionError:
                pos_offset=0
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
        redraw()
    def draw_children(self):
        for child in self.inputs: 
            child.draw()
        for child in self.outputs:
            child.draw()
    def destroy(self):
        inputs=self.inputs
        outputs=self.outputs
        conns=[]
        for connection in connections:
            conns.append(connection)
        for connection in conns:
            if connection.p1 in inputs or connection.p1 in outputs or connection.p2 in inputs or connection.p2 in outputs:
                connections.remove(connection)
        for connector in self.inputs:
            conn_points.remove(connector)
        for connector in self.outputs:
            conn_points.remove(connector)
        components.remove(self)

class AND(Component):
    def __init__(self,x,y):
        self.height=int(height/6)
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.smoothscale(and_surf,(self.width,self.height))
        inputs=[
            ConnectionPoint(-self.width/2,-self.height/4,self),
            ConnectionPoint(-self.width/2,self.height/4,self)
        ]
        outputs=[
            ConnectionPoint(self.width/2,0,self)
        ]
        super().__init__(x,y,inputs,outputs)
    def draw(self):
        super().draw_children()
        screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(self.x-(self.width/2),self.y-(self.height/2),self.width,self.height)))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(self.x-(self.width/2),self.y-(self.height/2),self.width,self.height),3)

class NAND(Component):
    def __init__(self,x,y):
        self.height=int(height/6)
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.smoothscale(nand_surf,(self.width,self.height))
        inputs=[
            ConnectionPoint(-self.width/2,-self.height/4,self),
            ConnectionPoint(-self.width/2,self.height/4,self)
        ]
        outputs=[
            ConnectionPoint(self.width/2,0,self)
        ]
        super().__init__(x,y,inputs,outputs)
    def draw(self):
        super().draw_children()
        screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(self.x-(self.width/2),self.y-(self.height/2),self.width,self.height)))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(self.x-(self.width/2),self.y-(self.height/2),self.width,self.height),3)

class OR(Component):
    def __init__(self,x,y):
        self.height=int(height/6)
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.smoothscale(or_surf,(self.width,self.height))
        inputs=[
            ConnectionPoint(-self.width/2,-self.height/4,self),
            ConnectionPoint(-self.width/2,self.height/4,self)
        ]
        outputs=[
            ConnectionPoint(self.width/2,0,self)
        ]
        super().__init__(x,y,inputs,outputs)
    def draw(self):
        super().draw_children()
        screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(self.x-(self.width/2),self.y-(self.height/2),self.width,self.height)))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(self.x-(self.width/2),self.y-(self.height/2),self.width,self.height),3)

class NOR(Component):
    def __init__(self,x,y):
        self.height=int(height/6)
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.smoothscale(nor_surf,(self.width,self.height))
        inputs=[
            ConnectionPoint(-self.width/2,-self.height/4,self),
            ConnectionPoint(-self.width/2,self.height/4,self)
        ]
        outputs=[
            ConnectionPoint(self.width/2,0,self)
        ]
        super().__init__(x,y,inputs,outputs)
    def draw(self):
        super().draw_children()
        screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(self.x-(self.width/2),self.y-(self.height/2),self.width,self.height)))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(self.x-(self.width/2),self.y-(self.height/2),self.width,self.height),3)

class NOT(Component):
    def __init__(self,x,y):
        self.height=int(height/6)
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.smoothscale(not_surf,(self.width,self.height))
        inputs=[
            ConnectionPoint(-self.width/2,0,self),
        ]
        outputs=[
            ConnectionPoint(self.width/2,0,self)
        ]
        super().__init__(x,y,inputs,outputs)
    def draw(self):
        super().draw_children()
        screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(self.x-(self.width/2),self.y-(self.height/2),self.width,self.height)))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(self.x-(self.width/2),self.y-(self.height/2),self.width,self.height),3)

def get_component_collision(x,y):
    for i in range(len(components)):
        component=components[len(components)-i-1]
        if component.x+(component.width)/2 > x and component.x-(component.width/2) < x:
            if component.y+(component.height)/2 > y and component.y-(component.height/2) < y:
                return component
    return None

def get_connector_collision(x,y):
    if get_component_collision(x,y) == None:
        for connector in conn_points:
            if math.sqrt(((connector.parent_obj.x+connector.rel_x-x)**2)+((connector.parent_obj.y+connector.rel_y-y)**2))<30:
                return connector
    return None

component_menu=None
last_connector=None

def redraw():
    global component_menu
    global components
    w,h=width,height
    screen.fill((255,255,255))
    for connection in connections:
        connection.draw()
    for component in components:
        component.draw()
    pygame.gfxdraw.box(screen,pygame.Rect(0,0,w,h/8),(0,0,0,100))
    component_menu=Menu(orientation.HORIZONTAL,w/2,h/16,w,h/8)
    component_menu.add_component(MenuComponent(component_menu,and_surf,lambda:(AND(int(w/2),int(h/2)).draw())))
    component_menu.add_component(MenuComponent(component_menu,or_surf,lambda:(OR(int(w/2),int(h/2)).draw())))
    component_menu.add_component(MenuComponent(component_menu,not_surf,lambda:(NOT(int(w/2),int(h/2)).draw())))
    component_menu.add_component(MenuComponent(component_menu,nor_surf,lambda:(NOR(int(w/2),int(h/2)).draw())))
    component_menu.add_component(MenuComponent(component_menu,nand_surf,lambda:(NAND(int(w/2),int(h/2)).draw())))
    if last_connector != None:
        pygame.gfxdraw.line(screen,int(last_connector.parent_obj.x+last_connector.rel_x),int(last_connector.parent_obj.y+last_connector.rel_y),int(pygame.mouse.get_pos()[0]),int(pygame.mouse.get_pos()[1]),(0,0,0,120))
    component_menu.draw()
    pygame.display.update()

last_x=0
last_y=0
drag=None

async def update_loop():
    global last_x
    global last_y
    global width
    global height
    global drag
    global last_connector
    while True:
        pygame.event.pump()
        if last_connector != None:
            redraw()
        for event in pygame.event.get():
            try:
                if event.type==pygame.VIDEORESIZE:
                    width, height = event.dict['size']
                    pygame.display.set_mode((width,height),pygame.RESIZABLE)
                    redraw()
                if event.type==pygame.MOUSEBUTTONDOWN:
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
                                        redraw()
                            else:
                                last_connector=connector
                        else:
                            last_connector=None
                            redraw()
                    if pygame.mouse.get_pressed()[2]:
                        if get_component_collision(event.pos[0],event.pos[1]) != None:
                            component=get_component_collision(event.pos[0],event.pos[1])
                            component.destroy()
                            redraw()
                if pygame.mouse.get_pressed()[0]:
                    delta_x=event.pos[0]-last_x
                    delta_y=event.pos[1]-last_y
                    if last_x != 0:
                        if drag==None:
                            if get_component_collision(event.pos[0],event.pos[1]) != None:
                                component=get_component_collision(event.pos[0],event.pos[1])
                                component.x+=delta_x
                                component.y+=delta_y
                                drag=component
                                redraw()
                        else:
                            drag.x+=delta_x
                            drag.y+=delta_y
                            redraw()
                    last_x=event.pos[0]
                    last_y=event.pos[1]
                if event.type==pygame.MOUSEBUTTONUP and event.button==1:
                    drag=None
                    last_x=0
                    last_y=0
            except AttributeError:
                pass
        await asyncio.sleep(0.016)
