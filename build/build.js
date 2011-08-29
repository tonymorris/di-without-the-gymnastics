var p = Ent.Project.create("webradardocument", "external");
p.setVersion(3, 1, 0);


p.setConfig({
    command: ["echo", ">", "/dev/null"],
    dist: "package"
});
