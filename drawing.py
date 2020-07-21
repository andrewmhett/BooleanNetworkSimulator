import sys
sys.path.append("/Library/Python/3.7/site-packages")
import pygame
import asyncio
pygame.display.init()
pygame.display.set_mode((800,800))
screen=pygame.display.get_surface()

and_surf=pygame.image.load("assets/AND.png")
or_surf=pygame.image.load("assets/OR.png")

components=[]

class ConnectionPoint:
    def __init__(self,rel_x,rel_y,parent):
        self.rel_x=rel_x
        self.rel_y=rel_y
        self.parent_obj=parent
    def draw(self):
       x=self.parent_obj.x+self.rel_x
       y=self.parent_obj.y+self.rel_y
       center=(int(self.parent_obj.x+self.rel_x),int(self.parent_obj.y+self.rel_y))
       pygame.draw.circle(screen,(0,0,0),center,self.parent_obj.height/25)

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

class AND(Component):
    def __init__(self,x,y):
        self.height=200
        self.width=int(1.66*(self.height))
        self.image=pygame.transform.scale(and_surf,(self.width,self.height))
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
        pygame.display.flip()

def get_component_collision(x,y):
    for i in range(len(components)):
        component=components[len(components)-i-1]
        if component.x+(component.width)/2 > x and component.x-(component.width/2) < x:
            if component.y+(component.height)/2 > y and component.y-(component.height/2) < y:
                return component
    return None

def redraw():
    screen.fill((255,255,255))
    for component in components:
        component.draw()

screen.fill((255,255,255))
pygame.display.flip()

last_x=0
last_y=0

async def update_loop():
    global last_x
    global last_y
    while True:
        pygame.event.pump()
        for event in pygame.event.get():
            if pygame.mouse.get_pressed()[0]:
                delta_x=event.pos[0]-last_x
                delta_y=event.pos[1]-last_y
                if last_x != 0:
                    if get_component_collision(event.pos[0],event.pos[1]) != None:
                        component=get_component_collision(event.pos[0],event.pos[1])
                        component.x+=delta_x
                        component.y+=delta_y
                        redraw()
                last_x=event.pos[0]
                last_y=event.pos[1]
            if event.type==pygame.MOUSEBUTTONUP and event.button==1:
                last_x=0
                last_y=0
        await asyncio.sleep(0.016)
