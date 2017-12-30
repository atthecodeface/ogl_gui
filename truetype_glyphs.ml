(*a Glyph module *)
    (*
    An outline font glyph has the following attributes:

    name - name of the glyph, not used internally
    unichr - unicode character that the glyph represents
    metrics - dictionary of key->value, keys of
      advance_width     - amount to move X on after the glyph is rendered
      left_side_bearing - X displacement for rendering (e.g. letter y may have -ve value)
      xMin              - X bounding box minimum value
      xMax              - X bounding box maximum value
      yMin              - Y bounding box minimum value
      yMax              - Y bounding box maximum value
    glyph - contours making the outline
    mesh - mesh of triangles created from bezier curves
     *)
    module Glyph = struct
    type t = {
    name : string;
    unichr: Uchar.t;
    triangles : ;
    metrics : ;
    glyph: ;
    mesh : (string * int) list ref ;
      }

    (* make - basically an init *)
    let make unichr name =
        { name; unichr; triangles=[]; metrics }

    (* ttx_get_element_by_name - TrueType get element by name - 'class' method
    let ttx_get_element_by_name node tag_name name =
        let fn acc m = 
            if m.getAttribute("name")==name then Some m else acc
        List.fold_left fn (None) node.getElementsByTagName(tag_name)
    *)
    (* ttx_get_int_attributes
    let ttx_get_int_attributes node name_map =
      let fn acc t_n = 
        let (t,n) = t_n in
        let x = node.getAttribute(t) in
        match x with
          None -> ((t,None)::acc)
        | Some s ->
           try let i = int_of_string s ; ((t,Some i)::acc)
               with _ -> ((t,None)::acc)
        List.fold_left fn [] name_map
     *)
    (* ttx_get_metrics 
       Read ttx element; extract int attributes as metrix: lsb is left-side-bearing;
       advance width is x-addition to next glyph
    let ttx_get_metrics hmtx t =
      match ttx_get_element_by_name hmtx "mtx" name with
        None -> t
      | Some g ->
         metrics := ttx_get_int_attributes glyph [("width","advance_width"), ("lsb","left_side_bearing"));
         t
    *)
    (* ttx_get_glyph *)
    def ttx_get_glyph( self, glyf ):
        """
        Get contours for a glyph and any other components

        Currently does not handle components
        <TTGlyph name="agrave" xMin="8" yMin="-15" xMax="386" yMax="658">
          <component glyphName="a" x="4" y="0" flags="0x4"/>
          <component glyphName="grave" x="189" y="-164" flags="0x4"/>
        </TTGlyph>
        """
        glyph = self.ttx_get_element_by_name(glyf, "TTGlyph", self.name)
        if glyph is None:
            return None
        r = self.ttx_get_int_attributes( glyph, {"xMin":"xMin", "yMin":"yMin", "xMax":"xMax", "yMax":"yMax"} )
        self.metrics["xMin"] = r["xMin"]
        self.metrics["xMax"] = r["xMax"]
        self.metrics["yMin"] = r["yMin"]
        self.metrics["yMax"] = r["yMax"]
        contours = []
        for c in glyph.getElementsByTagName("contour"):
            pts = []
            last = None
            def add_point(x,y,on,last,pts=pts):
                if last is not None:
                    if (not last["on"]) and (not on):
                        pts.append(((last["x"]+x)/2.0, (last["y"]+y)/2.0))
                        pass
                    elif (last["on"]) and (on):
                        pts.append(((last["x"]+x)/2.0, (last["y"]+y)/2.0))
                        pass
                pts.append( (x,y) )
                return  {"x":x, "y":y, "on":on}
            for p in c.getElementsByTagName("pt"):
                x  = int(p.getAttribute("x"))
                y  = int(p.getAttribute("y"))
                on = int(p.getAttribute("on"))
                if last is None:
                    first_point = (x,y,on)
                    pass
                last = add_point(x,y,on,last)
                pass
            last = add_point( first_point[0], first_point[1], first_point[2], last )
            contours.append(pts)
            pass
        self.glyph = contours
        pass
    (* create_bezier_lists *)
    def create_bezier_lists( self ):
        bezier_lists = []
        for c in self.glyph:
            beziers = []
            i = 0
            p0 = c0 = p1 = None
            while i<len(c):
                (p0,c0,p1) = (c0,p1,bezier.c_point(coords=c[i]))
                if ((i&1)==0) and p0 is not None:
                    beziers.append( bezier.c_bezier2( pts=(p0,c0,p1) ) )
                    pass
                i += 1
                pass
            bezier_lists.append(beziers)
            pass
        return bezier_lists
    (* create_straight_lines *)
    def create_straight_lines(self, straightness=50):
        lines = []
        contours = self.create_bezier_lists()
        for bl in contours:
            points = []
            lines.append(points)
            for b in bl:
                subbeziers = b.break_into_segments(straightness)
                for s in subbeziers:
                    points.append(s.pts[0])
                    pass
                pass
            pass
        return lines
    (* get_mesh *)
    def get_mesh( self, straightness=50 ):
        if self.mesh is not None: return self.mesh
        m = mesh.c_mesh()
        contours = self.create_bezier_lists()
        for bl in contours:
            m.add_bezier_list_contour( bezier_list=bl, closed=True, contour_data=None, straightness=straightness )
            pass
        m.map_contours_to_mesh()
        m.normalize()
        m.fill_convex_hull_with_triangles()
        m.remove_small_lines(min_length=0.01)
        m.remove_small_area_triangles( min_area=0.01)
        for i in range(10):
            if m.shorten_quad_diagonals()==0: break
            pass
        for i in range(10):
            m.ensure_contours_on_mesh()
            m.shorten_quad_diagonals()
            m.remove_small_lines(min_length=0.01)
            m.remove_small_area_triangles( min_area=0.01)
            pass
        m.assign_winding_order_to_contours()
        m.assign_winding_order_to_mesh()
        #m.check_consistent()
        self.mesh = m
        return m
    (* get_bbox *)
    def get_bbox( self ):
        if self.metrics["xMax"] is None: return (0,0,0,0)
        if self.metrics["xMin"] is None: return (0,0,0,0)
        if self.metrics["yMax"] is None: return (0,0,0,0)
        if self.metrics["yMin"] is None: return (0,0,0,0)
        lx = self.metrics["xMin"]
        w = self.metrics["xMax"] - self.metrics["xMin"]
        if w<0:
            w=-w
            lx = self.metrics["xMax"]
            pass
        by = self.metrics["yMin"]
        h = self.metrics["yMax"] - self.metrics["yMin"]
        if h<0:
            h=-h
            by = self.metrics["xMin"]
            pass
        return (lx,by,w,h)
    (* get_metrics *)
    def get_metrics( self ):
        r = {"lx":0, "by":0, "w":0, "h":0, "advance_width":0, "left_side_bearing":0}
        if "xMin" in self.metrics:
            r["lx"] = self.metrics["xMin"]
            if "xMax" in self.metrics:
                r["w"] = self.metrics["xMax"] - self.metrics["xMin"]
                pass
            pass
        if "yMin" in self.metrics:
            r["by"] = self.metrics["yMin"]
            if "yMax" in self.metrics:
                r["h"] = self.metrics["yMax"] - self.metrics["yMin"]
                pass
            pass
        if "advance_width" in self.metrics:
            r["advance_width"] = self.metrics["advance_width"]
            pass
        if "left_side_bearing" in self.metrics:
            r["left_side_bearing"] = self.metrics["left_side_bearing"]
            pass
        return r
    (* draw *)
    def draw(self, size=(60,60), metrics={"MaxWidth":1.0, "MaxHeight":1.0}, straightness=100):
        import draw
        d = draw.c_draw_buffer(size=size,mode="1")
        lines = self.create_straight_lines(straightness=straightness)
        scale = (size[0]/float(metrics["MaxWidth"]), size[1]/float(metrics["MaxHeight"]))
        offset = (-float(self.metrics["xMin"])*scale[0], -float(self.metrics["yMin"])*scale[1])
        scale = (scale[0],-scale[1])
        offset = (offset[0],size[1]-offset[1])
        paths = []
        for l in lines:
            paths.append([])
            for p in l:
                paths[-1].append(p.get_coords(offset=offset,scale=scale))
                pass
            pass
        d.fill_paths(paths=paths,value=255)
        return d
    #f __repr__
    def __repr__( self ):
        result = "glyph '%s' ('%s') : %s : %s"%(self.unichr,self.name,str(self.metrics),str(self.glyph))
        return result


    end

