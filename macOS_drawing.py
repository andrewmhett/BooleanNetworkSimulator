import sys
sys.path.append("/Library/Python/3.7/site-packages")
import pygame
import pygame.gfxdraw
import asyncio
import math
import os
import datetime
pygame.display.init()
pygame.font.init()

font=pygame.font.SysFont("Arial",120)
small_font=pygame.font.SysFont("Arial",80)

width=1000
height=800

scale_factor=1.6
x_offset=0
y_offset=0
framerate=144
pygame.display.set_mode((width,height),pygame.RESIZABLE)
screen=pygame.display.get_surface()
save_path=None
save_callback=None
load_callback=None
id_counter=0

comp_height=int(height/6)
comp_width=int(1.66*comp_height)

and_surf=pygame.image.load("assets/AND.png")
or_surf=pygame.image.load("assets/OR.png")
not_surf=pygame.image.load("assets/NOT.png")
nand_surf=pygame.image.load("assets/NAND.png")
nor_surf=pygame.image.load("assets/NOR.png")
button_up_surf=pygame.Surface((int(height/7),int(1.66*(height/7))))
button_down_surf=pygame.Surface((int(height/7),int(1.66*(height/7))))
pygame.draw.rect(button_up_surf,(0,0,0),pygame.Rect(int(height/28)-10.5,int(1.66*(height/28))-10.5,int(height/14)+20.5,int(1.66*(height/14))+20.5),4)
pygame.gfxdraw.rectangle(button_up_surf,pygame.Rect(int(height/28)-11,int(1.66*(height/28))-11,int(height/14)+21,int(1.66*(height/14))+21),(0,0,0,255))
pygame.gfxdraw.box(button_down_surf,pygame.Rect(int(height/28)-10,int(1.66*(height/28))-10,int(height/14)+20,int(1.66*(height/14))+20),(0,0,0,255))
high_bit_surf=pygame.image.load("assets/ONE.png")
save_surf=pygame.image.load("assets/SAVE.png")
load_surf=pygame.image.load("assets/LOAD.png")

components=[]
conn_points=[]
connections=[]

strTypeLookup={}

class orientation:
    HORIZONTAL=1
    VERTICAL=2

class IO:
    INPUT=1
    OUTPUT=2

class ConnectionPoint:
    def __init__(self,rel_x,rel_y,io,parent,**kwargs):
        global id_counter
        self.rel_x=rel_x
        self.rel_y=rel_y
        self.io=io
        self.parent_obj=parent
        self.state=False
        conn_points.append(self)
        id_counter+=1
        if 'id' in kwargs.keys():
            self.id=int(kwargs['id'])
        else:
            self.id=id_counter
    def draw(self):
       center=(int((self.parent_obj.x+self.rel_x)/scale_factor)+x_offset,int((self.parent_obj.y+self.rel_y)/scale_factor)+y_offset)
       color=(0,0,0) if (self.state==False) else (0,255,0)
       pygame.draw.circle(screen,color,center,0.5+(self.parent_obj.height/(12*scale_factor)))
       pygame.gfxdraw.aacircle(screen,center[0],center[1],1+int(self.parent_obj.height/(12*scale_factor)),(0,0,0))
    def dump(self):
        return {
            "category":"CONNECTION_POINT",
            "id":self.id,
            "io":"OUTPUT" if (self.io==IO.OUTPUT) else "INPUT",
            "parent_id":self.parent_obj.id
        }

class Connection:
    def __init__(self,p1,p2,**kwargs):
        global id_counter
        self.p1=p1
        self.p2=p2
        self.state=False
        connections.append(self)
        id_counter+=1
        if 'id' in kwargs.keys():
            self.id=int(kwargs['id'])
        else:
            self.id=id_counter
    def draw(self):
        p1=self.p1
        p2=self.p2
        p1_pos=((p1.parent_obj.x+p1.rel_x)/scale_factor)+x_offset,((p1.parent_obj.y+p1.rel_y)/scale_factor)+y_offset
        p2_pos=((p2.parent_obj.x+p2.rel_x)/scale_factor)+x_offset,((p2.parent_obj.y+p2.rel_y)/scale_factor)+y_offset
        color=((0,0,0) if self.state == False else (0,255,0))
        pygame.draw.aaline(screen,color,p1_pos,p2_pos,int(3*scale_factor))
    def dump(self):
        return {
            "category":"CONNECTION",
            "id":self.id,
            "p1_id":self.p1.id,
            "p2_id":self.p2.id
        }

class PromptType:
    TEXT=1
    LIST=2

class Prompt:
    def __init__(self,width,height,type,title):
        self.width=width
        self.height=height
        self.title=title
        self.type=type
        self.file_index=0
        self.file_array=[]
        for file in os.walk("saves"):
            self.file_array.append(file)
        self.file_array=self.file_array[0][2][1:]
        for i in range(len(self.file_array)):
            self.file_array[i]=self.file_array[i].split(".txt")[0]
        if self.type==PromptType.TEXT:
            self.input="" if (save_path==None) else save_path
        else:
            self.input=self.file_array[0]
    def draw(self):
        pygame.gfxdraw.box(screen,pygame.Rect(int((width/2)-(self.width/2)),int((height/2)-(self.height/2)),self.width,self.height),(0,0,0,200))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(int((width/2)-(self.width/2)),int((height/2)-(self.height/2)),self.width,self.height),3)
        screen.blit(small_font.render(self.title,True,(255,255,255)),(int((width/2)-int(small_font.render(self.title,True,(255,255,255)).get_size()[0]/2)),int((height/2)-(self.height/2)+30)))
        screen.blit(font.render(self.input,True,(0,0,0),(255,255,255)),pygame.draw.rect(screen,(255,255,255),pygame.Rect(int((width/2)-(self.width/2)+20),int((height/2)+(self.height/2)-(self.height/3)-30),self.width-40,self.height/3)))
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(int((width/2)-(self.width/2)+20),int((height/2)+(self.height/2)-(self.height/3)-30),self.width-40,self.height/3),3)
    def get_input(self):
        if self.type==PromptType.TEXT:
            while True:
                for event in pygame.event.get():
                    if event.type==pygame.KEYDOWN:
                        if event.key==13:
                            return self.input
                        elif event.key==27:
                            return None
                        elif event.key==8:
                            self.input=self.input[0:len(self.input)-1]
                            redraw()
                        else:
                            key=pygame.key.name(event.key)
                            if len(key)==1:
                                if font.render(self.input+key,True,(0,0,0),(255,255,255)).get_size()[0] <= self.width-40:
                                    self.input+=key
                                    redraw()
        if self.type==PromptType.LIST:
            while True:
                for event in pygame.event.get():
                    if event.type==pygame.KEYDOWN:
                        if event.key==1073741904:
                            if self.file_index>0:
                                self.file_index-=1
                                self.input=self.file_array[self.file_index]
                                redraw()
                        if event.key==1073741903:
                            if self.file_index<len(self.file_array)-1:
                                self.file_index+=1
                                self.input=self.file_array[self.file_index]
                                redraw()
                        if event.key==13:
                            return self.input
                        if event.key==27:
                            return None

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
            if self.orientation==orientation.HORIZONTAL:
                try:
                    pos_offset=component.width*len(self.components)/2
                except ZeroDivisionError:
                    pos_offset=0
                if len(self.components)%2==0:
                    pos_offset-=component.width/2
                counter+=1
                comp_x=self.x-pos_offset+(counter*(component.width))
                comp_y=self.y
                if comp_x-int(component.width/2) < x and comp_x+int(component.width/2) > x:
                    if comp_y-int(component.height/2) < y and comp_y+int(component.height/2) > y:   
                        return component
            if self.orientation==orientation.VERTICAL:
                try:
                    pos_offset=component.height*len(self.components)/2
                except ZeroDivisionError:
                    pos_offset=0
                if len(self.components)%2==0:
                    pos_offset-=component.height/2
                counter+=1
                comp_y=self.y-pos_offset+(counter*(component.height))
                comp_x=self.x
                if comp_x-int(component.width/2) < x and comp_x+int(component.width/2) > x:
                    if comp_y-int(component.height/2) < y and comp_y+int(component.height/2) > y:   
                        return component
        return None

class MenuComponent():
    def __init__(self,menu,surf,callback,**kwargs):
        self.menu=menu
        self.callback=callback
        self.height=int(height/15)
        self.width=int(1.66*(self.height))
        if "width_div" in kwargs.keys():
            self.width=int(self.width/kwargs['width_div'])
        if "height_div" in kwargs.keys():
            self.height=int(self.height/kwargs['height_div'])
        self.image=pygame.transform.smoothscale(surf,(self.width,self.height))
    def draw(self,position):
        if self.menu.orientation == orientation.HORIZONTAL:
            try:
                pos_offset=self.width*len(self.menu.components)/2
            except ZeroDivisionError:
                pos_offset=0
            if len(self.menu.components)%2==0:
                pos_offset-=self.width/2
            if self.menu.x-pos_offset+(position*(self.width))-int(self.width/2)>0:
                screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(self.menu.x-pos_offset+(position*(self.width))-int(self.width/2),self.menu.y-int(self.height/2),self.width,self.height)))
            pygame.draw.rect(screen,(0,0,0),pygame.Rect(self.menu.x-pos_offset+(position*(self.width))-int(self.width/2),self.menu.y-int(self.height/2),self.width,self.height),3)
        if self.menu.orientation==orientation.VERTICAL:
            try:
                pos_offset=self.height*len(self.menu.components)/2
            except ZeroDivisionError:
                pos_offset=0
            if len(self.menu.components)%2==0:
                pos_offset-=self.height/2
            if self.menu.y-pos_offset+(position*(self.height))-int(self.height/2)>0:
                screen.blit(self.image,pygame.draw.rect(screen,(255,255,255),pygame.Rect(self.menu.x-int(self.width/2),self.menu.y-pos_offset+(position*(self.height))-int(self.height/2),self.width,self.height)))
            pygame.draw.rect(screen,(0,0,0),pygame.Rect(self.menu.x-int(self.width/2),self.menu.y-pos_offset+(position*(self.height))-int(self.height/2),self.width,self.height),3)

class Component:
    def __init__(self,x,y,inputs,outputs,**kwargs):
        global id_counter
        self.inputs=inputs
        self.outputs=outputs
        self.x=x
        self.y=y
        components.append(self)
        id_counter+=1
        if 'id' in kwargs.keys():
            self.id=int(kwargs['id'])
        else:
            self.id=id_counter
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
    def draw(self):
        self.draw_children()
        self.image=pygame.transform.smoothscale(self.surf,(int(self.width/scale_factor),int(self.height/scale_factor)))
        surface_rect=pygame.draw.rect(screen,self.background_color,pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor)))
        if int((self.x-(self.width/2))/scale_factor)+x_offset>0 and int((self.y-(self.height/2))/scale_factor)+y_offset>0:
            screen.blit(self.image,surface_rect)
        pygame.draw.rect(screen,(0,0,0),pygame.Rect(int((self.x-(self.width/2))/scale_factor)+x_offset,int((self.y-(self.height/2))/scale_factor)+y_offset,int(self.width/scale_factor),int(self.height/scale_factor)),3)
    def dump(self):
        return {
            "category":"COMPONENT",
            "id":self.id,
            "type":strTypeLookup[type(self)],
            "x":self.x,
            "y":self.y
        }

class AND(Component):
    def __init__(self,x,y,**kwargs):
        self.background_color=(255,255,255)
        self.width=comp_width
        self.height=comp_height
        self.image=pygame.transform.smoothscale(and_surf,(self.width,self.height))
        self.surf=and_surf
        self.inputs=[
            ConnectionPoint(-self.width/2,-self.height/4,IO.INPUT,self),
            ConnectionPoint(-self.width/2,self.height/4,IO.INPUT,self)
        ]
        self.outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        super().__init__(x,y,self.inputs,self.outputs,**kwargs)
    

class NAND(Component):
    def __init__(self,x,y,**kwargs):
        self.background_color=(255,255,255)
        self.width=comp_width
        self.height=comp_height
        self.image=pygame.transform.smoothscale(nand_surf,(self.width,self.height))
        self.surf=nand_surf
        self.inputs=[
            ConnectionPoint(-self.width/2,-self.height/4,IO.INPUT,self),
            ConnectionPoint(-self.width/2,self.height/4,IO.INPUT,self)
        ]
        self.outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        super().__init__(x,y,self.inputs,self.outputs,**kwargs)

class OR(Component):
    def __init__(self,x,y,**kwargs):
        self.background_color=(255,255,255)
        self.width=comp_width
        self.height=comp_height
        self.image=pygame.transform.smoothscale(or_surf,(self.width,self.height))
        self.surf=or_surf
        self.inputs=[
            ConnectionPoint(-self.width/2,-self.height/4,IO.INPUT,self),
            ConnectionPoint(-self.width/2,self.height/4,IO.INPUT,self)
        ]
        self.outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        super().__init__(x,y,self.inputs,self.outputs,**kwargs)
 
class NOR(Component):
    def __init__(self,x,y,**kwargs):
        self.background_color=(255,255,255)
        self.width=comp_width
        self.height=comp_height
        self.image=pygame.transform.smoothscale(nor_surf,(self.width,self.height))
        self.surf=nor_surf
        self.inputs=[
            ConnectionPoint(-self.width/2,-self.height/4,IO.INPUT,self),
            ConnectionPoint(-self.width/2,self.height/4,IO.INPUT,self)
        ]
        self.outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        super().__init__(x,y,self.inputs,self.outputs,**kwargs)

class NOT(Component):
    def __init__(self,x,y,**kwargs):
        self.background_color=(255,255,255)
        self.width=comp_width
        self.height=comp_height
        self.image=pygame.transform.smoothscale(not_surf,(self.width,self.height))
        self.surf=not_surf
        self.inputs=[
            ConnectionPoint(-self.width/2,0,IO.INPUT,self)
        ]
        self.outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        super().__init__(x,y,self.inputs,self.outputs,**kwargs)

class BUTTON(Component):
    def __init__(self,x,y,**kwargs):
        self.background_color=(255,255,255)
        self.width=comp_width
        self.height=comp_height
        self.pressed=False
        self.image=pygame.transform.smoothscale(button_up_surf,(self.width,self.height))
        self.surf=button_up_surf
        self.inputs=[
            ConnectionPoint(-self.width/2,0,IO.INPUT,self)
        ]
        self.outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        super().__init__(x,y,self.inputs,self.outputs,**kwargs)
    def draw(self):
        super().draw_children()
        if self.pressed:
            self.surf=button_down_surf
        else:
            self.surf=button_up_surf
        super().draw()

class HIGH_BIT(Component):
    def __init__(self,x,y,**kwargs):
        self.background_color=(255,255,255)
        self.width=comp_width
        self.height=comp_height
        self.image=pygame.transform.smoothscale(high_bit_surf,(self.width,self.height))
        self.surf=high_bit_surf
        self.inputs=[]
        self.outputs=[
            ConnectionPoint(self.width/2,0,IO.OUTPUT,self)
        ]
        self.outputs[0].state=True
        super().__init__(x,y,self.inputs,self.outputs,**kwargs)

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
file_io_menu=None
save_button=None
load_button=None

strTypeLookup={
    AND:"AND",
    OR:"OR",
    NOT:"NOT",
    NAND:"NAND",
    NOR:"NOR",
    BUTTON:"BUTTON",
    HIGH_BIT:"HIGH_BIT"
}

current_prompt=None

def redraw():
    global component_menu
    global file_io_menu
    global components
    global save_button
    global load_button
    w,h=width,height
    screen.fill((255,255,255))
    if last_connector != None:
        pygame.draw.aaline(screen,(0,0,0),(int((last_connector.parent_obj.x+last_connector.rel_x)/scale_factor)+x_offset,int((last_connector.parent_obj.y+last_connector.rel_y)/scale_factor)+y_offset),(int(pygame.mouse.get_pos()[0]),int(pygame.mouse.get_pos()[1])),int(2*scale_factor))
    for connection in connections:
        connection.draw()
    for component in components:
        component.draw()
    pygame.gfxdraw.box(screen,pygame.Rect(0,0,w,h/12),(0,0,0,100))
    component_menu=Menu(orientation.HORIZONTAL,w/2,h/24,w,h/12)
    component_menu.add_component(MenuComponent(component_menu,and_surf,lambda:(AND(int(w/2)+x_offset,int(h/2)+y_offset).draw())))
    component_menu.add_component(MenuComponent(component_menu,or_surf,lambda:(OR(int(w/2)+x_offset,int(h/2)+y_offset).draw())))
    component_menu.add_component(MenuComponent(component_menu,not_surf,lambda:(NOT(int(w/2)+x_offset,int(h/2)+y_offset).draw())))
    component_menu.add_component(MenuComponent(component_menu,nor_surf,lambda:(NOR(int(w/2)+x_offset,int(h/2)+y_offset).draw())))
    component_menu.add_component(MenuComponent(component_menu,nand_surf,lambda:(NAND(int(w/2)+x_offset,int(h/2)+y_offset).draw())))
    component_menu.add_component(MenuComponent(component_menu,button_up_surf,lambda:(BUTTON(int(w/2)+x_offset,int(h/2)+y_offset).draw())))
    component_menu.add_component(MenuComponent(component_menu,high_bit_surf,lambda:(HIGH_BIT(int(w/2)+x_offset,int(h/2)+y_offset).draw())))
    component_menu.draw()
    file_io_menu=Menu(orientation.VERTICAL,w/18,h/24,w/9,h/12)
    save_button=MenuComponent(file_io_menu,save_surf,lambda:(),height_div=1.8,width_div=1.8)
    load_button=MenuComponent(file_io_menu,load_surf,lambda:(),height_div=1.8,width_div=1.8)
    file_io_menu.add_component(save_button)
    file_io_menu.add_component(load_button)
    file_io_menu.draw()
    if current_prompt != None:
        current_prompt.draw()
    pygame.display.update()

last_x=0
last_y=0
drag=[]
moved=False
last_w=width
lsat_h=height
last_resize=datetime.datetime.now()
resized=True

async def update_loop():
    global current_prompt
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
    global save_button
    global load_button
    global save_path
    global file_selection
    global components
    global conn_points
    global connections
    global last_w
    global last_h
    global last_resize
    global resized
    while True:
        pygame.event.pump()
        if save_path != None:
            pygame.display.set_caption("BooleanNetworkSimulator - {0}".format(save_path))
        else:
            pygame.display.set_caption("BooleanNetworkSimulator")
        if (datetime.datetime.now()-last_resize).microseconds>0 and resized:
                resized=False
                pygame.display.set_mode((width,height),pygame.RESIZABLE)
        for event in pygame.event.get():
            if event.type==pygame.QUIT:
                pygame.quit()
                exit(0)
            if event.type==pygame.VIDEORESIZE:
                if event.dict['size'][0] != width or event.dict['size'][1] != height:
                    resized=True
                    width, height = event.dict['size']
                    last_resize=datetime.datetime.now()
            if event.type==pygame.MOUSEBUTTONDOWN:
                if event.button == 4:
                    scale_factor=scale_factor*1.1
                if event.button == 5:
                    scale_factor=scale_factor/1.1
                if pygame.mouse.get_pressed()[0]:
                    if get_component_collision(event.pos[0],event.pos[1]) != None:
                        component=get_component_collision(event.pos[0],event.pos[1])
                        if component not in drag:
                            component.background_color=(200,200,200)
                            drag.append(component)
                    if component_menu.get_component_collision(event.pos[0],event.pos[1]) != None:
                        component=component_menu.get_component_collision(event.pos[0],event.pos[1])
                        component.callback()
                    if file_io_menu.get_component_collision(event.pos[0],event.pos[1]) != None:
                        component=file_io_menu.get_component_collision(event.pos[0],event.pos[1])
                        if component == save_button:
                            current_prompt=Prompt(int(width/1.8),int(height/3),PromptType.TEXT,"Save file name:")
                            redraw()
                            filename=current_prompt.get_input()
                            if filename != None:
                                save_path=filename
                                save_callback(save_path)
                            current_prompt=None
                        if component == load_button:
                            current_prompt=Prompt(int(width/1.8),int(height/3),PromptType.LIST,"Select file to load:")
                            redraw()
                            filename = current_prompt.get_input()
                            if filename != None:
                                save_path=filename
                                components=[]
                                conn_points=[]
                                connections=[]
                                load_callback(save_path)
                            current_prompt=None
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
                    if len(drag)==0:
                        if get_component_collision(event.pos[0],event.pos[1]) != None:
                            component=get_component_collision(event.pos[0],event.pos[1])
                            component.destroy()
                    else:
                        for dragged in drag:
                            dragged.destroy()
            if event.type==pygame.MOUSEBUTTONUP:
                if pygame.mouse.get_pressed(0):
                    if not moved:
                        if get_component_collision(event.pos[0],event.pos[1]) != None:
                            component=get_component_collision(event.pos[0],event.pos[1])
                            if not pygame.key.get_pressed()[1073742049]:
                                if isinstance(component,BUTTON):
                                    if not component.pressed:
                                        component.pressed=True
                                    else:
                                        component.pressed=False
                    else:
                        moved=False
            if pygame.mouse.get_pressed()[0]:
                try:
                    delta_x=event.pos[0]-last_x
                    delta_y=event.pos[1]-last_y
                    if last_x != 0:
                        if get_component_collision(event.pos[0],event.pos[1]) != None:
                            component=get_component_collision(event.pos[0],event.pos[1])
                            moved=True
                        else:
                            if len(drag)==0:
                                x_offset+=delta_x
                                y_offset+=delta_y
                        for dragged in drag:
                            dragged.x+=delta_x*scale_factor
                            dragged.y+=delta_y*scale_factor
                    last_x=event.pos[0]
                    last_y=event.pos[1]
                except AttributeError:
                    pass
            if event.type==pygame.KEYUP:
                if event.key==1073742049:
                    for dragged in drag:
                        dragged.background_color=(255,255,255)
                    drag=[]
            if event.type==pygame.MOUSEBUTTONUP and event.button==1:
                if not pygame.key.get_pressed()[1073742049]:
                    for dragged in drag:
                        dragged.background_color=(255,255,255)
                    drag=[]
                last_x=0
                last_y=0
        await asyncio.sleep(1/framerate)

async def draw_loop():
    while True:
        redraw()
        await asyncio.sleep(1/framerate)
