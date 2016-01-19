/*
 * Plot3D - simple 3d plotting
 *
 * Copyright (c) 2014 Grzegorz Mazur, Marta Noga
 *
 * Licensed under the MIT license:
 *   http://www.opensource.org/licenses/mit-license.php
 */

function Plot3D(series, w, h ) {
    var self = this;
    
    self.width = w;
    self.height = h;
    
    self.labels = [];
    var data = [];

    for (var s = 0; s < series.length; ++s)
        data.push(series[s]["data"]);

    self.xmin = data[0][0][0];
    self.xmax = data[0][0][0];

    self.ymin = data[0][0][1];
    self.ymax = data[0][0][1];

    self.zmin = data[0][0][2];
    self.zmax = data[0][0][2];

    for (var s = 0; s < series.length; ++s) {
        for (var i = 1; i < data[s].length; ++i) {
            if (data[s][i][0] < self.xmin)
                self.xmin = data[s][i][0];
            if (data[s][i][0] > self.xmax)
                self.xmax = data[s][i][0];
            if (data[s][i][1] < self.ymin)
                self.ymin = data[s][i][1];
            if (data[s][i][1] > self.ymax)
                self.ymax = data[s][i][1];
            if (data[s][i][2] < self.zmin)
                self.zmin = data[s][i][2];
            if (data[s][i][2] > self.zmax)
                self.zmax = data[s][i][2];
        }
    }

    if (self.zmin === self.zmax) {
        self.zmin -= 1;
        self.zmax += 1;
    }

    self.size = 500;
    self.zsize = 300;

    self.xscale = self.size / (self.xmax - self.xmin);
    self.yscale = self.size / (self.ymax - self.ymin);
    self.zscale = self.zsize / (self.zmax - self.zmin);

    self.xoffset = -self.size / 2;
    self.yoffset = -self.size / 2;
    self.zoffset = -self.zsize / 2;

    self.scene = new THREE.Scene();

    self.camera = new THREE.PerspectiveCamera(45, w / h, 0.1, 2000);

    self.camera.up.set(0, 0, 1);
    self.camera.position.set(155, -772, 1);
    self.camera.lookAt(self.scene.position);

    for (var s = 0; s < series.length; ++s) {
        var geometry = new THREE.Geometry();

        var triangles = Delaunay.triangulate(data[s]);

        for (var i = 0; i < triangles.length; i = i + 3) {
            var p0 = data[s][triangles[i]];
            var p1 = data[s][triangles[i + 1]];
            var p2 = data[s][triangles[i + 2]];

            geometry.vertices.push(self.graphToWorld(p0[0], p0[1], p0[2]));
            geometry.vertices.push(self.graphToWorld(p1[0], p1[1], p1[2]));
            geometry.vertices.push(self.graphToWorld(p2[0], p2[1], p2[2]));

            geometry.faces.push(new THREE.Face3(geometry.vertices.length - 3, geometry.vertices.length - 2, geometry.vertices.length - 1));
        }

        var noColors = self.colors.length;

        colorFront = self.colors[s % noColors];
        colorBack = self.shadeColor2(colorFront, -0.3);
        colorWireFrame = self.shadeColor2(colorFront, -0.6);

        var mf = new THREE.MeshBasicMaterial({
            color: colorBack,
            side: THREE.FrontSide
        });

        var mb = new THREE.MeshBasicMaterial({
            color: colorFront,
            side: THREE.BackSide
        });

        var wf = new THREE.MeshBasicMaterial({
            color: colorWireFrame,
            wireframe: true,
            transparent: true,
            side: THREE.DoubleSide});

        var mesh = THREE.SceneUtils.createMultiMaterialObject(geometry, [mf, mb, wf]);

        self.scene.add(mesh);
        self.labels.push(series[s]["label"]);
    }

    material = new THREE.LineBasicMaterial({color: 0x000000});
    geometry = new THREE.Geometry();

    geometry.vertices.push(new THREE.Vector3(-self.size / 2, -self.size / 2, -self.zsize / 2));
    geometry.vertices.push(new THREE.Vector3(self.size / 2, -self.size / 2, -self.zsize / 2));

    var line = new THREE.Line(geometry, material);

    self.scene.add(line);

    geometry = new THREE.Geometry();

    geometry.vertices.push(new THREE.Vector3(-self.size / 2, -self.size / 2, -self.zsize / 2));
    geometry.vertices.push(new THREE.Vector3(-self.size / 2, self.size / 2, -self.zsize / 2));

    line = new THREE.Line(geometry, material);

    self.scene.add(line);

    geometry = new THREE.Geometry();

    geometry.vertices.push(new THREE.Vector3(-self.size / 2, -self.size / 2, -self.zsize / 2));
    geometry.vertices.push(new THREE.Vector3(-self.size / 2, -self.size / 2, self.zsize / 2));

    var line = new THREE.Line(geometry, material);

    self.scene.add(line);

    var xParams = self.axisParams(self.xmin, self.xmax, 10);

    for (var t = xParams.b; t <= xParams.e; t += xParams.d) {
        var tb = self.graphToWorld(t, self.ymin, self.zmin);
        tb.y = -self.size / 2;
        tb.z = -self.zsize / 2;
        var te = tb.clone();
        te.y += self.size / 50;
        geometry = new THREE.Geometry();
        geometry.vertices.push(tb);
        geometry.vertices.push(te);
        te = tb.clone();
        te.z += self.zsize / 50;
        geometry.vertices.push(tb);
        geometry.vertices.push(te);
        var line = new THREE.Line(geometry, material);
        self.scene.add(line);

        var tn = new Number(t);
        var l = self.label(tn.toFixed(2), 0);
        l.position.set(tb.x, -self.size / 2, -self.zsize / 2);
        self.scene.add(l);
    }

    var yParams = self.axisParams(self.ymin, self.ymax, 10);

    for (var t = yParams.b; t <= yParams.e; t += yParams.d) {
        var tb = self.graphToWorld(self.xmin, t, self.zmin);
        tb.x = -self.size / 2;
        tb.z = -self.zsize / 2;
        var te = tb.clone();
        te.x += self.size / 50;
        geometry = new THREE.Geometry();
        geometry.vertices.push(tb);
        geometry.vertices.push(te);
        te = tb.clone();
        te.z += self.zsize / 50;
        geometry.vertices.push(tb);
        geometry.vertices.push(te);
        var line = new THREE.Line(geometry, material);
        self.scene.add(line);

        if (t !== yParams.b) {
            var tn = new Number(t);
            var l = self.label(tn.toFixed(2), 0);
            l.position.set(-self.size / 2, tb.y, -self.zsize / 2);
            self.scene.add(l);
        }
    }

    var zParams = self.axisParams(self.zmin, self.zmax, 5);

    for (var t = zParams.b; t <= zParams.e; t += zParams.d) {
        var tb = self.graphToWorld(self.xmin, self.ymin, t);
        tb.x = -self.size / 2;
        tb.y = -self.size / 2;
        var te = tb.clone();
        te.x += self.size / 50;
        geometry = new THREE.Geometry();
        geometry.vertices.push(tb);
        geometry.vertices.push(te);
        te = tb.clone();
        te.y += self.size / 50;
        geometry.vertices.push(tb);
        geometry.vertices.push(te);
        var line = new THREE.Line(geometry, material);
        self.scene.add(line);

        var tn = new Number(t);
        var l = self.label(tn.toFixed(2), 0);
        l.position.set(-self.size / 2, -self.size / 2, tb.z + 40);
        self.scene.add(l);
    }

};

Plot3D.prototype.setRenderer = function( webGLEnabled ){
    
    if (( webGLEnabled && Detector.webgl ))
        this.renderer = new THREE.WebGLRenderer({antialias: true});
    else
        this.renderer = new THREE.CanvasRenderer();
    
    this.renderer.setClearColor(0xffffff, 1);
    this.renderer.setSize(this.width , this.height);
    
};


Plot3D.prototype.colors = ["#edc240", "#afd8f8", "#cb4b4b", "#4da74d", "#9440ed"];

Plot3D.prototype.graphToWorld = function (x, y, z) {
    return new THREE.Vector3((x - this.xmin) * this.xscale + this.xoffset, (y - this.ymin) * this.yscale + this.yoffset, (z - this.zmin) * this.zscale + this.zoffset);
};

Plot3D.prototype.addLegend = function (placeholder) {
    legendDiv = $("<div class='legend'></div>").appendTo(placeholder);
    table = $("<table style='position:absolute;top:13px;right:13px;;font-size:smaller;color:#545454'></table>").appendTo(legendDiv);

    for (i = 0; i < this.labels.length; i++) {
        row = $("<tr></tr>");
        colorBoxTd = $("<td class='legendColorBox'><div style='border:1px solid #ccc;padding:1px'><div style='width:4px;height:0;border:5px solid " + this.colors[i % this.colors.length] + ";overflow:hidden'></div></div></td>");
        labelTd = $("<td class='legendLabel'>" + this.labels[i] + "</td></tr>");

        $(row).append(colorBoxTd);
        $(row).append(labelTd);
        $(row).appendTo(table);
    }

    var div = legendDiv.children();
    $("<div style='position:absolute;width:" + div.width() + "px;height:" + div.height() + "px; top: 13px; right: 13px; background-color: rgb(255, 255, 255); opacity: 0.85;'> </div>").prependTo(legendDiv);
};


Plot3D.prototype.axisParams = function (min, max, noTicks) {
    var delta = max - min;

    var scale = Math.pow(10, Math.floor((Math.round(Math.log(delta) / Math.LN10 * 1e6) / 1e6) - 1));
    var b = Math.floor(min / scale) * scale;
    var e = Math.ceil(max / scale) * scale;
    var d = Math.floor((e - b) / (noTicks * scale)) * scale;

    return {b: b, e: e, d: d};
};

Plot3D.prototype.label = function (text, color) {
    var canvas = document.createElement('canvas');
    var context = canvas.getContext('2d');
    var fontSize = 256;
    context.font = "normal " + fontSize + "px Arial";

    var textWidth = context.measureText(text).width;
    var textHeight = 200;

    canvas.width = textWidth;

    context.textAlign = "center";
    context.textBaseline = "middle";
    context.fillStyle = "#" + Number(0x1000000 + color).toString(16).substring(1);
    context.fillText(text, textWidth / 2, textHeight / 2);

    var texture = new THREE.Texture(canvas);
    texture.needsUpdate = true;

    var material = new THREE.SpriteMaterial({
        map: texture,
        useScreenCoordinates: false,
        transparent: true
    });

    var sprite = new THREE.Sprite(material);
    sprite.scale.set(textWidth / textHeight * fontSize, fontSize, 1);

    return sprite;
};

Plot3D.prototype.resizePlot = function (width, height) {

    this.camera.aspect = width / height;
    this.camera.updateProjectionMatrix();

    this.renderer.setSize(width, height);

};

Plot3D.prototype.shadeColor2 = function (color, percent) {
    var f = f = parseInt(color.slice(1), 16),
            t = percent < 0 ? 0 : 255,
            p = percent < 0 ? percent * -1 : percent,
            R = f >> 16,
            G = f >> 8 & 0x00FF,
            B = f & 0x0000FF;
    return "#" + (0x1000000 + (Math.round((t - R) * p) + R) * 0x10000 + (Math.round((t - G) * p) + G) * 0x100 + (Math.round((t - B) * p) + B)).toString(16).slice(1);
};

function isWebGLSupported(){

    if ( Detector.webgl == false )
        return false;
    return true;
}

